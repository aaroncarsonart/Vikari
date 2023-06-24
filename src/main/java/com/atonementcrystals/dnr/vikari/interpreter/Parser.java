package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.RightAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.PrintExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.PrintStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.SyntaxErrorStatement;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.PrintStatementOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.NegateCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.Vikari_FieldMemberExistsException;
import com.atonementcrystals.dnr.vikari.error.Vikari_ParserException;
import com.atonementcrystals.dnr.vikari.error.Vikari_UndefinedFieldMemberException;
import com.atonementcrystals.dnr.vikari.interpreter.resolver.TypeResolver;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import com.atonementcrystals.dnr.vikari.util.TokenPosition;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Parses the output of the Lexer into an abstract syntax tree consisting of
 * a list of statements.
 */
public class Parser {
    private static final Logger log = LogManager.getLogger(Parser.class);

    private File file;
    private List<List<AtonementCrystal>> lexedStatements;
    private int lineNumber;
    private int tokenNumber;

    private int lineCount;
    private int lastLineLength;

    private List<AtonementCrystal> currentLine;

    private SyntaxErrorReporter syntaxErrorReporter;
    private TypeResolver typeResolver;

    /** Parent field of the rootEnvironment. */
    private AtonementField globalAtonementField;

    /** Root environments are unique per source file. */
    private Map<String, AtonementField> rootEnvironments;
    private AtonementField rootEnvironment;
    private AtonementField currentEnvironment;

    public Parser() {
        typeResolver = new TypeResolver();
        rootEnvironments = new HashMap<>();
    }

    public void setSyntaxErrorReporter(SyntaxErrorReporter syntaxErrorReporter) {
        this.syntaxErrorReporter = syntaxErrorReporter;
    }

    public void setGlobalAtonementField(AtonementField globalAtonementField) {
        this.globalAtonementField = globalAtonementField;
    }

    public AtonementField getRootEnvironment() {
        return rootEnvironment;
    }

    public List<Statement> parse(File file, List<List<AtonementCrystal>> lexedStatements) {
        log.trace("parse({})", file == null ? "null" : "\"" + file + "\"");
        this.file = file;

        // Short-circuit if no statements exist to parse.
        List<Statement> statements = new ArrayList<>();
        if (lexedStatements.isEmpty()) {
            return statements;
        }

        // First pass on user input.
        if (lineNumber == 0) {
            this.lexedStatements = lexedStatements;
            establishRootEnvironment();
        }

        // REPL mode is reusing the Parser state.
        else {
            this.lexedStatements.addAll(lexedStatements);
        }

        lineCount = this.lexedStatements.size();
        lastLineLength = this.lexedStatements.get(this.lexedStatements.size() - 1).size();

        // Parse the file.
        while (!isAtEnd()) {
            currentLine = this.lexedStatements.get(lineNumber);
            do {
                Statement statement = statement();
                statements.add(statement);
            } while (!isAtEndOfStatement());
            advanceToNextLine();
        }

        // Visit the Resolvers.
        typeResolver.resolve(statements);
        typeResolver.reportErrors(syntaxErrorReporter, file);

        this.file = null;
        return statements;
    }

    /**
     * Environments are used in the Parser to detect variable declarations
     * and to ensure that variables are declared before they are used.
     */
    private void establishRootEnvironment() {
        // File is for a type or a script.
        // Cache the environment with the file path.
        if (file != null) {
            String filePath = file.getAbsolutePath();
            if (rootEnvironments.containsKey(filePath)) {
                rootEnvironment = rootEnvironments.get(filePath);
            } else {
                rootEnvironment = new AtonementField(globalAtonementField);
                rootEnvironments.put(filePath, rootEnvironment);
            }
        }

        // Code is executed with -c or the REPL.
        // So only one environment is necessary.
        else if (rootEnvironment == null) {
            rootEnvironment = new AtonementField(globalAtonementField);
        }
        currentEnvironment = rootEnvironment;
    }

    private Statement statement() {
        try {
            if (checkVariableDeclaration()) {
                return variableDeclarationStatement();
            }
            if (check(TokenType.TYPE_LABEL)) {
                return printStatement();
            }
            return expressionStatement();
        } catch (Vikari_ParserException e) {
            synchronize();

            List<AtonementCrystal> lexedStatement = getLastVisitedLexedStatement();
            String statementString = lexedStatement.stream()
                    .map(AtonementCrystal::getIdentifier)
                    .collect(Collectors.joining(""));

            CoordinatePair statementLocation = lexedStatement.get(0).getCoordinates();

            SyntaxErrorStatement syntaxErrorStatement = new SyntaxErrorStatement(statementString);
            syntaxErrorStatement.setLocation(statementLocation);
            return syntaxErrorStatement;
        }
    }

    private boolean checkVariableDeclaration() {
        if (check(ReferenceCrystal.class)) {
            AtonementCrystal reference = peek();
            String identifier = reference.getIdentifier();

            // We know it is a variable declaration if it is undefined.
            if (!currentEnvironment.hasFieldMember(identifier)) {
                return true;
            }

            // Parse as a variable declaration for the error case.
            return lookAhead(1) instanceof TypeLabelOperatorCrystal;
        }
        return false;
    }

    private Statement variableDeclarationStatement() {
        ReferenceCrystal variableToDefine = consume(ReferenceCrystal.class, "Expected a variable reference.");

        // Check for an optional type label.
        TypeReferenceCrystal typeReference;
        TypeCrystal declaredType = null;

        if (match(TokenType.TYPE_LABEL)) {
            typeReference = consume(TypeReferenceCrystal.class, "Expected type reference after type label operator.");
            String typeName = typeReference.getIdentifier();

            // Fetch the crystal definition from the environment.
            try {
                declaredType = (TypeCrystal) rootEnvironment.get(typeName);
            } catch (Vikari_UndefinedFieldMemberException e) {
                // TODO: Will need to move this check to the TypeResolver.
                error(typeReference, "Unknown Type.");
                declaredType = VikariType.ATONEMENT_CRYSTAL.getTypeCrystal();
            } catch (ClassCastException e) {
                throw new Vikari_ParserException("Internal error. Type identifier not mapped to a TypeCrystal.");
            }
        }

        if (declaredType == null) {
            declaredType = VikariType.ATONEMENT_CRYSTAL.getTypeCrystal();
        }
        variableToDefine.setDeclaredType(declaredType);

        // Set up the final expression statement.
        VariableDeclarationStatement declarationStatement;

        // Handle initializing to a non-default value.
        if (match(TokenType.LEFT_ASSIGNMENT)) {
            BinaryOperatorCrystal operator = (BinaryOperatorCrystal) previous();
            Expression initializerExpression = expression();
            declarationStatement = new VariableDeclarationStatement(variableToDefine, declaredType, operator,
                    initializerExpression);
        }

        // Case for not specifying a default value.
        else {
            declarationStatement = new VariableDeclarationStatement(variableToDefine, declaredType, null, null);
        }

        // Define the crystal in the current scope. (For checking use of undefined crystals.)
        String identifierToDefine = variableToDefine.getIdentifier();
        try {
            currentEnvironment.define(identifierToDefine, variableToDefine);
        } catch (Vikari_FieldMemberExistsException e) {
            error(variableToDefine, "Variable is already defined.");
        }

        if (!isAtEndOfStatement()) {
            error(peek(), "Unexpected token(s) in variable declaration statement.");
        }

        // Advance past an optional statement separator , crystal.
        // (And synchronize after an error case.)
        advanceToEndOfStatement();

        CoordinatePair location = variableToDefine.getCoordinates();
        declarationStatement.setLocation(location);
        return declarationStatement;
    }

    private Statement printStatement() {
        List<PrintExpression> printExpressions = new ArrayList<>();
        int currentLine = lineNumber;
        while (match(TokenType.TYPE_LABEL)) {
            AtonementCrystal typeLabel = previous();
            PrintStatementOperatorCrystal printOperator = new PrintStatementOperatorCrystal();
            printOperator.setCoordinates(typeLabel.getCoordinates());

            Expression expr = null;
            if (currentLine == lineNumber && !check(TokenType.TYPE_LABEL) && !check(TokenType.STATEMENT_SEPARATOR)
                    && !isAtEnd() && !isAtEndOfStatement()) {
                expr = expression();
            }

            PrintExpression printExpression = new PrintExpression(printOperator, expr);
            printExpression.setLocation(printOperator.getCoordinates());
            printExpressions.add(printExpression);
        }
        PrintStatement printStatement = new PrintStatement(printExpressions);
        CoordinatePair location = printExpressions.get(0).getLocation();
        printStatement.setLocation(location);

        // Advance past an optional statement separator , crystal.
        match(TokenType.STATEMENT_SEPARATOR);

        return printStatement;
    }

    private Statement expressionStatement() {
        Expression expression = expression();

        if (!isAtEndOfStatement()) {
            error(peek(), "Expected expression.");
        }

        ExpressionStatement expressionStatement = new ExpressionStatement(expression);
        expressionStatement.setLocation(expression.getLocation());

        // Advance past an optional statement separator , crystal.
        // (And synchronize after an error case.)
        advanceToEndOfStatement();

        return expressionStatement;
    }

    private Expression expression() {
        return assignment();
    }

    private Expression assignment() {
        Expression left = termExpression();

        if (match(TokenType.LEFT_ASSIGNMENT)) {
            BinaryOperatorCrystal operator = (BinaryOperatorCrystal) previous();
            Expression right = assignment();

            if (!(left instanceof VariableExpression)) {
                error(left.getLocation(), "Invalid target for assignment expression.");
            }

            LeftAssignmentExpression assignmentExpression = new LeftAssignmentExpression(left, operator, right);
            return assignmentExpression;
        }

        if (match(TokenType.CONTINUE)) {
            BinaryOperatorCrystal operator = new RightAssignmentOperatorCrystal();
            operator.setCoordinates(previous().getCoordinates());

            Expression right = assignment();

            if (!(right instanceof VariableExpression)) {
                error(right.getLocation(), "Invalid target for assignment expression.");
            }

            RightAssignmentExpression assignmentExpression = new RightAssignmentExpression(left, operator, right);
            return assignmentExpression;
        }

        return left;
    }

    private Expression termExpression() {
        Expression left = factorExpression();
        CoordinatePair location = left.getLocation();

        while (match(TokenType.ADD, TokenType.SUBTRACT)) {
            BinaryOperatorCrystal operator = (BinaryOperatorCrystal) previous();
            Expression right = factorExpression();
            left = new BinaryExpression(left, operator, right);
            left.setLocation(location);
        }

        return left;
    }

    private Expression factorExpression() {
        Expression left = unaryExpression();
        CoordinatePair location = left.getLocation();

        while (match(TokenType.MULTIPLY, TokenType.LEFT_DIVIDE, TokenType.RIGHT_DIVIDE)) {
            BinaryOperatorCrystal operator = (BinaryOperatorCrystal) previous();
            Expression right = unaryExpression();
            left = new BinaryExpression(left, operator, right);
            left.setLocation(location);
        }

        return left;
    }

    private Expression unaryExpression() {
        if (match(TokenType.SUBTRACT)) {
            AtonementCrystal previous = previous();
            NegateCrystal operator = new NegateCrystal();
            operator.setCoordinates(previous.getCoordinates());

            Expression right = unaryExpression();
            UnaryExpression unaryExpression = new UnaryExpression(operator, right);
            unaryExpression.setLocation(operator.getCoordinates());

            return unaryExpression;
        }

        return primary();
    }

    private Expression primary() {
        // Number literal
        if (match(NumberCrystal.class)) {
            AtonementCrystal previous = previous();
            LiteralExpression literalExpression = new LiteralExpression(previous);
            literalExpression.setLocation(previous.getCoordinates());
            return literalExpression;
        }

        // Grouping expression
        if (match(TokenType.LEFT_SQUARE_BRACKET)) {
            AtonementCrystal bracket = previous();
            CoordinatePair location = bracket.getCoordinates();

            Expression expression = expression();
            consume(TokenType.RIGHT_SQUARE_BRACKET, "Expected `]` after expression.");

            GroupingExpression groupingExpression = new GroupingExpression(expression);
            groupingExpression.setLocation(location);
            return groupingExpression;
        }

        // Variable reference
        if (match(ReferenceCrystal.class)) {
            AtonementCrystal reference = previous();
            String identifier = reference.getIdentifier();
            if (!currentEnvironment.isDefined(identifier)) {
                error(reference, "Undefined variable reference.");
            } else {
                // Since assignments aren't processed yet, need to fetch the declared type.
                AtonementCrystal fieldMember = currentEnvironment.get(identifier);
                reference.setDeclaredType(fieldMember.getDeclaredType());
                reference.setField(fieldMember.getField());
            }
            return new VariableExpression(reference);
        }

        AtonementCrystal errorCrystal;
        if (isAtEndOfStatement() || isAtEnd()) {
            errorCrystal = previous();
        } else {
            errorCrystal = peek();
        }

        throw error(errorCrystal, "Expected expression.");
    }

    public boolean isAtEndOfStatement(int tokenNumber) {
        if (tokenNumber >= currentLine.size()) {
            return true;
        }
        AtonementCrystal currentToken = currentLine.get(tokenNumber);
        return TokenType.STATEMENT_SEPARATOR.getJavaType().isInstance(currentToken);
    }

    public boolean isAtEndOfStatement() {
        return check(TokenType.STATEMENT_SEPARATOR) || tokenNumber >= currentLine.size();
    }

    public boolean isAtEnd(TokenPosition position) {
        return isAtEnd(position.getLineNumber(), position.getTokenNumber());
    }

    public boolean isAtEnd(int tokenNumber) {
        return isAtEnd(lineNumber, tokenNumber);
    }

    public boolean isAtEnd(int lineNumber, int tokenNumber) {
        return lineNumber >= lineCount || (
                (lineNumber == lineCount - 1) &&
                        (tokenNumber >= lastLineLength));
    }

    public boolean isAtEnd() {
        return isAtEnd(lineNumber, tokenNumber);
    }

    public boolean match(TokenType... tokenTypes) {
        for (TokenType tokenType : tokenTypes) {
            if (check(tokenType)) {
                advance();
                return true;
            }
        }
        return false;
    }

    public boolean match(Class<? extends AtonementCrystal>... crystalTypes) {
        for (Class<? extends AtonementCrystal> crystalType : crystalTypes) {
            if (check(crystalType)) {
                advance();
                return true;
            }
        }
        return false;
    }

    private AtonementCrystal lookAhead(int distance) {
        int position = tokenNumber;
        AtonementCrystal current = currentLine.get(position);
        for (int i = 0; i < distance; i++) {
            position++;

            if (!isAtEnd(position) && !isAtEndOfStatement(position)) {
                current = currentLine.get(position);
            }
        }
        return current;
    }

    public AtonementCrystal advance() {
        AtonementCrystal previous = peek();
        if (!isAtEnd()) {
            tokenNumber++;
        }
        return previous;
    }

    public void advanceToEndOfLine() {
        tokenNumber = currentLine.size();
    }

    public void advanceToEndOfStatement() {
        while (tokenNumber < currentLine.size()) {
            if (match(TokenType.STATEMENT_SEPARATOR)) {
                break;
            }
            advance();
        }
    }

    public void advanceToNextLine() {
        ++lineNumber;
        tokenNumber = 0;
        if (isAtEnd()) {
            currentLine = null;
        } else {
            currentLine = lexedStatements.get(lineNumber);
        }
    }

    public boolean check(TokenType tokenType) {
        if (isAtEnd()) {
            return false;
        }
        AtonementCrystal crystal = peek();
        return tokenType.getJavaType().isInstance(crystal);
    }

    public boolean check(Class<? extends AtonementCrystal> crystalType) {
        if (isAtEnd()) {
            return false;
        }
        AtonementCrystal crystal = peek();
        return crystalType.isInstance(crystal);
    }

    public boolean check(AtonementCrystal crystal, TokenType... tokenTypes) {
        if (crystal == null) {
            return false;
        }
        for (TokenType tokenType : tokenTypes) {
            if (tokenType.getJavaType().isInstance(crystal)) {
                return true;
            }
        }
        return false;
    }

    public boolean check(AtonementCrystal crystal, Class<? extends AtonementCrystal>... crystalTypes) {
        if (crystal == null) {
            return false;
        }
        for (Class<? extends AtonementCrystal> crystalType : crystalTypes) {
            if (crystalType.isInstance(crystal)) {
                return true;
            }
        }
        return false;
    }

    private AtonementCrystal peek() {
        if (tokenNumber >= currentLine.size()) {
            return null;
        }
        return lexedStatements.get(lineNumber).get(tokenNumber);
    }

    private AtonementCrystal previous() {
        TokenPosition position = new TokenPosition(lineNumber, tokenNumber);
        position = backup(position);

        int lineNumber = position.getLineNumber();
        int tokenNumber = position.getTokenNumber();
        return lexedStatements.get(lineNumber).get(tokenNumber);
    }

    private AtonementCrystal get(TokenPosition position) {
        int lineNumber = position.getLineNumber();
        int tokenNumber = position.getTokenNumber();
        if (isAtEnd(lineNumber, tokenNumber)) {
            return null;
        }
        List<AtonementCrystal> lexedStatement = lexedStatements.get(lineNumber);
        AtonementCrystal crystal = lexedStatement.get(tokenNumber);
        return crystal;
    }

    private TokenPosition backup(TokenPosition position) {
        int lineNumber = position.getLineNumber();
        int tokenNumber = position.getTokenNumber();
        if (tokenNumber == 0) {
            lineNumber--;
            List<AtonementCrystal> prevStatement = lexedStatements.get(lineNumber);
            tokenNumber = prevStatement.size() - 1;
        } else {
            tokenNumber--;
        }
        return new TokenPosition(lineNumber, tokenNumber);
    }

    private AtonementCrystal consume(TokenType tokenType, String errorMessage) {
        return consume(tokenType.getJavaType(), errorMessage);
    }

    private <T extends AtonementCrystal> T consume(Class<T> crystalType, String errorMessage) {
        if (check(crystalType)) {
            return (T) advance();
        }
        AtonementCrystal errorCrystal;
        if (isAtEndOfStatement() || isAtEnd()) {
            CoordinatePair location = previous().getCoordinates();
            errorCrystal = new AtonementCrystal("");
            int row = location.getRow();
            int nextCol = location.getColumn() + 1;
            errorCrystal.setCoordinates(new CoordinatePair(row, nextCol));
        } else {
            errorCrystal = peek();
        }
        throw error(errorCrystal, errorMessage);
    }

    private Vikari_ParserException error(AtonementCrystal crystal, String errorMessage) {
        CoordinatePair location = crystal.getCoordinates();
        return error(location, errorMessage);
    }

    private Vikari_ParserException error(CoordinatePair location, String errorMessage) {
        SyntaxError syntaxError = new SyntaxError(file, location, errorMessage);
        syntaxErrorReporter.add(syntaxError);
        return new Vikari_ParserException(errorMessage);
    }

    private void synchronize() {
        // TODO: Will need special handling for the LINE_CONTINUATION operator ~.
        advanceToEndOfStatement();
    }

    private List<AtonementCrystal> getLastVisitedLexedStatement() {
        List<AtonementCrystal> lastLine = currentLine;
        if (currentLine == null) {
            lastLine = lexedStatements.get(lexedStatements.size() - 1);
        }
        return lastLine;
    }

    public void clear() {
        file = null;
        lexedStatements = null;
        lineNumber = 0;
        tokenNumber = 0;
        lineCount = 0;
        lastLineLength = 0;
        currentLine = null;
        rootEnvironment = null;
        currentEnvironment = null;
    }
}
