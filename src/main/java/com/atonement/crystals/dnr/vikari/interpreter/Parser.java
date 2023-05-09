package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonement.crystals.dnr.vikari.core.crystal.literal.number.DoubleLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.literal.number.LongLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.PrintStatementOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.NegateCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.separator.WhitespaceCrystal;
import com.atonement.crystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonement.crystals.dnr.vikari.core.expression.Expression;
import com.atonement.crystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonement.crystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonement.crystals.dnr.vikari.core.expression.PrintExpression;
import com.atonement.crystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonement.crystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonement.crystals.dnr.vikari.core.statement.PrintStatement;
import com.atonement.crystals.dnr.vikari.core.statement.Statement;
import com.atonement.crystals.dnr.vikari.core.statement.SyntaxErrorStatement;
import com.atonement.crystals.dnr.vikari.error.SyntaxError;
import com.atonement.crystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonement.crystals.dnr.vikari.error.Vikari_ParserException;
import com.atonement.crystals.dnr.vikari.util.CoordinatePair;
import com.atonement.crystals.dnr.vikari.util.TokenPosition;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
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
    private int statementNumber;

    private List<AtonementCrystal> currentLine;

    private SyntaxErrorReporter syntaxErrorReporter;

    public Parser() {
    }

    public void setSyntaxErrorReporter(SyntaxErrorReporter syntaxErrorReporter) {
        this.syntaxErrorReporter = syntaxErrorReporter;
    }

    public List<Statement> parse(File file, List<List<AtonementCrystal>> lexedStatements) {
        log.trace("parse()");
        this.file = file;
        this.lexedStatements = lexedStatements;
        lineCount = lexedStatements.size();
        lastLineLength = lexedStatements.get(lexedStatements.size() - 1).size();

        List<Statement> statements = new ArrayList<>();
        while (!isAtEnd()) {
            // TODO: Fix statement number. Error case is stepping over the next line.
            currentLine = lexedStatements.get(lineNumber);
            Statement statement = statement();
            statementNumber = statement.getLocation().getRow();
            statements.add(statement);
            advanceToNextLine();
        }
        return statements;
    }

    private Statement statement() {
        try {
            if (check(TokenType.TYPE_LABEL)) {
                return printStatement();
            }
            return expressionStatement();
        } catch (Vikari_ParserException e) {
            synchronize();

            // TODO: Fix statement number. Error case is stepping over the next line.
            List<AtonementCrystal> lastVisitedLine = getLastVisitedLexedStatement();
            String statementString = lastVisitedLine.stream()
                    .map(AtonementCrystal::getIdentifier)
                    .collect(Collectors.joining(""));

            SyntaxErrorStatement syntaxErrorStatement = new SyntaxErrorStatement(statementString);
            int errorLineNumber = statementNumber - 1;
            syntaxErrorStatement.setLocation(new CoordinatePair(errorLineNumber, 0));
            return syntaxErrorStatement;
        }
    }

    private Statement printStatement() {
        List<PrintExpression> printExpressions = new ArrayList<>();
        int currentLine = lineNumber;
        while (match(TokenType.TYPE_LABEL)) {
            AtonementCrystal typeLabel = previous();
            PrintStatementOperatorCrystal printOperator = new PrintStatementOperatorCrystal();
            printOperator.setCoordinates(typeLabel.getCoordinates());

            Expression expr = null;
            if (currentLine == lineNumber && !check(TokenType.TYPE_LABEL) && !isAtEnd() && !isAtEndOfStatement()) {
                expr = expression();
            }

            PrintExpression printExpression = new PrintExpression(printOperator, expr);
            printExpression.setLocation(printOperator.getCoordinates());
            printExpressions.add(printExpression);
        }
        PrintStatement printStatement = new PrintStatement(printExpressions);
        CoordinatePair location = printExpressions.get(0).getLocation();
        printStatement.setLocation(location);

        return printStatement;
    }

    private Statement expressionStatement() {
        Expression expression = expression();

        if (!isAtEndOfStatement()) {
            error(peek(), "Expected expression.");
        }

        ExpressionStatement expressionStatement = new ExpressionStatement(expression);
        expressionStatement.setLocation(expression.getLocation());

        return expressionStatement;
    }

    private Expression expression() {
        return termExpression();
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
        if (match(LongLiteralCrystal.class, DoubleLiteralCrystal.class)) {
            AtonementCrystal previous = previous();
            LiteralExpression literalExpression = new LiteralExpression(previous);
            literalExpression.setLocation(previous.getCoordinates());
            return literalExpression;
        }
        if (match(TokenType.LEFT_SQUARE_BRACKET)) {
            AtonementCrystal bracket = previous();
            CoordinatePair location = bracket.getCoordinates();

            Expression expression = expression();
            consume(TokenType.RIGHT_SQUARE_BRACKET, "Expected `]` after expression.");

            GroupingExpression groupingExpression = new GroupingExpression(expression);
            groupingExpression.setLocation(location);
            return groupingExpression;
        }
        AtonementCrystal errorCrystal;
        if (isAtEndOfStatement() || isAtEnd()) {
            errorCrystal = previous();
        } else {
            errorCrystal = peek();
        }

        throw error(errorCrystal, "Expected expression.");
    }

    public boolean isAtEndOfStatement() {
        return tokenNumber >= currentLine.size();
    }

    public boolean isAtEnd(TokenPosition position) {
        return isAtEnd(position.getLineNumber(), position.getTokenNumber());
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

    public AtonementCrystal advance() {
        AtonementCrystal previous = peek();
        if (!isAtEnd()) {
            tokenNumber++;

            // Advance past all whitespace that is not indentation.
            if (tokenNumber > 0 && check(WhitespaceCrystal.class)) {
                advance();
            }
        }
        return previous;
    }

    public void advanceToEndOfLine() {
        tokenNumber = currentLine.size();
    }

    public void advanceToNextLine() {
        ++lineNumber;
        tokenNumber = 0;
        if (isAtEnd()) {
            currentLine = null;
            statementNumber = lexedStatements.size() - 1;
        } else {
            currentLine = lexedStatements.get(lineNumber);
            statementNumber++;
        }
    }

    public boolean check(TokenType tokenType) {
        if (isAtEnd()) {
            return false;
        }
        AtonementCrystal crystal = peek();
        return tokenType.getType().isInstance(crystal);
    }

    public boolean check(Class<? extends AtonementCrystal> crystalType) {
        if (isAtEnd()) {
            return false;
        }
        AtonementCrystal crystal = peek();
        return crystalType.isInstance(crystal);
    }

    private AtonementCrystal peek() {
        if (tokenNumber >= currentLine.size()) {
            return null;
        }
        return lexedStatements.get(lineNumber).get(tokenNumber);
    }

    private AtonementCrystal previous() {
        // Walk backwards to the first non-whitespace crystal.
        TokenPosition position = new TokenPosition(lineNumber, tokenNumber);
        do {
            position = backup(position);
        } while (isAtEnd(position) || get(position) instanceof WhitespaceCrystal);

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
        if (check(tokenType)) {
            return advance();
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

        List<AtonementCrystal> lastVisitedLine = getLastVisitedLexedStatement();
        String lineString = lastVisitedLine.stream().map(AtonementCrystal::getIdentifier).collect(Collectors.joining());
        SyntaxError syntaxError = new SyntaxError(file, location, lineString, errorMessage);
        syntaxErrorReporter.add(syntaxError);
        return new Vikari_ParserException(errorMessage);
    }

    private void synchronize() {
        // TODO: Will need special handling for the LINE_CONTINUATION operator ~.

        advanceToEndOfLine();
    }

    private List<AtonementCrystal> getLastVisitedLexedStatement() {
        List<AtonementCrystal> lastLine = currentLine;
        if (currentLine == null) {
            lastLine = lexedStatements.get(lexedStatements.size() - 1);
        }
        return lastLine;
    }
}
