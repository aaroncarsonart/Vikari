package com.atonementcrystals.dnr.vikari.parser;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeHierarchy;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.RightAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.interpreter.VikariProgram;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static com.atonementcrystals.dnr.vikari.TestUtils.assertSyntaxErrors;
import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.*;
import static org.junit.jupiter.api.Assertions.*;

public class ParserTest_Base {
    protected final AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();
    protected AtonementField rootEnvironment;
    protected SyntaxErrorReporter syntaxErrorReporter;

    protected List<Statement> lexAndParse(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        parser.setGlobalAtonementField(globalAtonementField);

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);
        rootEnvironment = parser.getRootEnvironment();

        return parsedStatements;
    }

    protected List<Statement> lexAndParse(String sourceString, int expectedErrorCount) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        parser.setGlobalAtonementField(globalAtonementField);

        this.syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertSyntaxErrors(syntaxErrorReporter, expectedErrorCount);
        rootEnvironment = parser.getRootEnvironment();

        return parsedStatements;
    }

    protected List<Statement> lexAndParse_WithErrors(String sourceString, int expectedErrorCount) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        parser.setGlobalAtonementField(globalAtonementField);

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertSyntaxErrors(syntaxErrorReporter, expectedErrorCount);
        rootEnvironment = parser.getRootEnvironment();

        return parsedStatements;
    }

    protected void testDeclaration(Statement statement, String identifier, VikariType declaredType,
                                   VikariType instantiatedType, CoordinatePair location) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, instantiatedType, location);
        assertNull(declarationStatement.getAssignmentOperator(), "Expected operator to be null.");
        assertNull(declarationStatement.getInitializerExpression(), "Expected initializer expression to be null.");
    }

    public void testDeclaration(Statement statement, String identifier, VikariType declaredType,
                                VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, instantiatedType, location);

        // test operator
        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        ParserTest_Utils.testRvalue(value, literal, instantiatedType);
    }

    /**
     * Test the form of a redeclaration of a variable. Such statements should still be parsed and well-formed.
     */
    public void testDeclaration_DuplicateError(Statement statement, String identifier, VikariType declaredType,
                                               VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, instantiatedType, location, null);

        // test operator
        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        ParserTest_Utils.testRvalue(value, literal, instantiatedType);
    }

    /**
     * Test a declaration where the assignment from a literal value does not match the type of the declared variable.
     */
    public void testDeclaration_TypeError(Statement statement, String identifier, VikariType declaredType,
                                          VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, VikariType.INVALID, location, null);

        // test operator
        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        ParserTest_Utils.testRvalue(value, literal, instantiatedType);
    }

    /**
     * Test a declaration where the assignment from another variable does not match the type of the declared variable.
     */
    public void testDeclaration_FromVariable_TypeError(Statement statement, String identifier, VikariType declaredType,
                                                       CoordinatePair location, String initializerIdentifier,
                                                       VikariType initializerDeclaredType,
                                                       VikariType initializerInstantiatedType,
                                                       CoordinatePair initializerLocation) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, VikariType.INVALID, location, null);

        // test operator
        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(VariableExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        // NOTE: The parameter localEnvironment is null because the variable is not tested for existing
        //       in the localEnvironment as each unique variable reference is a different instance.
        AtonementCrystal initializerVariable = ((VariableExpression) initializerExpression).getReference();
        testVariableCrystal(initializerVariable, initializerIdentifier, initializerDeclaredType,
                initializerInstantiatedType, initializerLocation, null);
    }

    protected void testDeclaration_NullKeyword(Statement statement, String identifier, VikariType declaredType,
                                               CoordinatePair variableLocation, CoordinatePair initializerLocation) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(variableLocation, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, variableLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        testNullKeyword(initializerExpression, initializerLocation);
    }

    protected void testDeclaration_NullSwordLiteral(Statement statement, String identifier, VikariType declaredType,
                                                    CoordinatePair variableLocation, CoordinatePair initializerLocation,
                                                    int expectedLength) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(variableLocation, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, variableLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        testNullSwordLiteral(initializerExpression, initializerLocation, expectedLength);
    }

    protected void testDeclaration_NullLiteralExpression(Statement statement, String identifier,
                                                         VikariType declaredType, CoordinatePair variableLocation,
                                                         CoordinatePair initializerLocation,
                                                         CoordinatePair operandLocation, int expectedLength) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(variableLocation, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, variableLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        testNullLiteralExpression_SingleIntegerOperand(initializerExpression, initializerLocation, operandLocation,
                expectedLength);
    }

    protected void testVariableCrystal(AtonementCrystal variable, String expectedIdentifier,
                                    VikariType expectedDeclaredType, VikariType expectedInstantiatedType,
                                    CoordinatePair expectedCoordinates) {

        testVariableCrystal(variable, expectedIdentifier, expectedDeclaredType,
                expectedInstantiatedType, expectedCoordinates, rootEnvironment);
    }

    protected void testVariableCrystal(AtonementCrystal variable, String expectedIdentifier,
                                           VikariType expectedDeclaredType, VikariType expectedInstantiatedType,
                                           CoordinatePair expectedCoordinates, AtonementField localEnvironment) {
        // Check variable from expression
        assertEquals(expectedIdentifier, variable.getIdentifier(), "Unexpected variable identifier.");
        assertEquals(expectedDeclaredType.getTypeCrystal(), variable.getDeclaredType(), "Unexpected declared type.");

        if (expectedInstantiatedType == VikariType.NULL) {
            TypeCrystal nullType = TypeHierarchy.getNullTypeFor(expectedDeclaredType);
            assertEquals(nullType, variable.getInstantiatedType(), "Unexpected instantiated null type.");
        } else {
            assertEquals(expectedInstantiatedType.getTypeCrystal(), variable.getInstantiatedType(), "Unexpected instantiated type.");
        }

        assertEquals(expectedCoordinates, variable.getCoordinates(), "Unexpected coordinates.");

        // Check variable exists in local environment
        if (localEnvironment != null && expectedDeclaredType != VikariType.INVALID) {
            assertTrue(localEnvironment.isDefined(expectedIdentifier), "Expected variable to be defined in local environment.");
            assertTrue(localEnvironment.hasFieldMember(expectedIdentifier), "Expected variable to be a field member of the local environment.");

            AtonementCrystal fieldMember = localEnvironment.get(expectedIdentifier);
            assertEquals(variable.getIdentifier(), fieldMember.getIdentifier(), "Expected equal identifiers.");
            assertEquals(variable.getDeclaredType(), fieldMember.getDeclaredType(), "Expected equal declared types.");
        }
    }

    public void testVariableExpression(Statement statement, String identifier, VikariType declaredType,
                                       VikariType instantiatedType, CoordinatePair location) {

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        assertEquals(location, statement.getLocation(), "Unexpected location.");

        Expression innerExpression = expressionStatement.getExpression();
        testVariableExpression(innerExpression, identifier, declaredType, instantiatedType, location);
    }

    public void testVariableExpression(Expression expression, String identifier, VikariType declaredType,
                                       VikariType instantiatedType, CoordinatePair location) {

        assertEquals(VariableExpression.class, expression.getClass(), "Unexpected expression type.");

        AtonementCrystal variable = ((VariableExpression) expression).getReference();
        testVariableCrystal(variable, identifier, declaredType, instantiatedType, location);
    }

    protected void testSimpleLeftAssignment(Statement statement, String identifier, VikariType declaredType,
                                          VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(location, statement.getLocation(), "Unexpected statement location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        Expression expression = ((ExpressionStatement) statement).getExpression();

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected statement type.");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        Expression lvalueExpression = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalueExpression.getClass(), "Unexpected lvalue type.");

        AtonementCrystal lvalue = ((VariableExpression) lvalueExpression).getReference();
        testVariableCrystal(lvalue, identifier, declaredType, instantiatedType, location);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        if (value != null) {
            AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
            ParserTest_Utils.testRvalue(value, rvalue, instantiatedType);
        }
    }

    /**
     * Type errors expect a NULL instantiated type on the lvalue. But the type of the rvalue still needs to be checked.
     */
    protected void testSimpleLeftAssignment_TypeError(Statement statement, String identifier, VikariType declaredType,
                                                    VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(location, statement.getLocation(), "Unexpected statement location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        Expression expression = ((ExpressionStatement) statement).getExpression();

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected statement type.");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        Expression lvalueExpression = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalueExpression.getClass(), "Unexpected lvalue type.");

        AtonementCrystal lvalue = ((VariableExpression) lvalueExpression).getReference();
        testVariableCrystal(lvalue, identifier, declaredType, VikariType.INVALID, location);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        ParserTest_Utils.testRvalue(value, rvalue, instantiatedType);
    }

    /**
     * Type errors expect a NULL instantiated type on the lvalue. But the type of the rvalue still needs to be checked.
     */
    protected void testLeftAssignment_FromVariable_TypeError(Statement statement, String lvalueIdentifier,
                                                           VikariType declaredType, CoordinatePair lvalueLocation,
                                                           String rvalueIdentifier, VikariType rvalueDeclaredType,
                                                           VikariType rvalueInstantiatedType,
                                                           CoordinatePair rvalueLocation) {

        assertEquals(lvalueLocation, statement.getLocation(), "Unexpected statement location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        Expression expression = ((ExpressionStatement) statement).getExpression();

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected statement type.");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        Expression lvalueExpression = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalueExpression.getClass(), "Unexpected lvalue type.");

        AtonementCrystal lvalue = ((VariableExpression) lvalueExpression).getReference();
        testVariableCrystal(lvalue, lvalueIdentifier, declaredType, VikariType.INVALID, lvalueLocation);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(VariableExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((VariableExpression) rvalueExpression).getReference();
        testVariableCrystal(rvalue, rvalueIdentifier, rvalueDeclaredType, rvalueInstantiatedType, rvalueLocation);
    }

    protected void testLeftAssignment_NullKeyword(Statement statement, String identifier, VikariType declaredType,
                                                  CoordinatePair lvalueLocation, CoordinatePair rvalueLocation) {

        assertEquals(lvalueLocation, statement.getLocation(), "Unexpected location.");
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        Expression expression = expressionStatement.getExpression();
        assertEquals(lvalueLocation, expression.getLocation(), "Unexpected location.");

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected expression type");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        // test variable
        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, lvalueLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression rvalue = assignmentExpression.getRvalue();
        testNullKeyword(rvalue, rvalueLocation);
    }

    protected void testLeftAssignment_NullSwordLiteral(Statement statement, String identifier, VikariType declaredType,
                                                       CoordinatePair lvalueLocation,
                                                       CoordinatePair rvalueLocation, int expectedLength) {

        assertEquals(lvalueLocation, statement.getLocation(), "Unexpected location.");
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        Expression expression = expressionStatement.getExpression();
        assertEquals(lvalueLocation, expression.getLocation(), "Unexpected location.");

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected expression type");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        // test variable
        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, lvalueLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression rvalue = assignmentExpression.getRvalue();
        testNullSwordLiteral(rvalue, rvalueLocation, expectedLength);
    }

    protected void testLeftAssignment_NullLiteralExpression(Statement statement, String identifier,
                                                            VikariType declaredType, CoordinatePair lvalueLocation,
                                                            CoordinatePair rvalueLocation,
                                                            CoordinatePair operandLocation, int expectedLength) {

        assertEquals(lvalueLocation, statement.getLocation(), "Unexpected location.");
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        Expression expression = expressionStatement.getExpression();
        assertEquals(lvalueLocation, expression.getLocation(), "Unexpected location.");

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected expression type");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        // test variable
        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, lvalueLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression rvalue = assignmentExpression.getRvalue();
        testNullLiteralExpression_SingleIntegerOperand(rvalue, rvalueLocation, operandLocation, expectedLength);
    }

    protected void testSimpleRightAssignment(Statement statement, CoordinatePair statementLocation, String identifier,
                                           VikariType declaredType, VikariType instantiatedType,
                                           CoordinatePair lvalueLocation, Object value) {

        assertEquals(statementLocation, statement.getLocation(), "Unexpected statement location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        Expression expression = ((ExpressionStatement) statement).getExpression();

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected statement type.");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        Expression lvalueExpression = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalueExpression.getClass(), "Unexpected lvalue type.");

        AtonementCrystal lvalue = ((VariableExpression) lvalueExpression).getReference();
        testVariableCrystal(lvalue, identifier, declaredType, instantiatedType, lvalueLocation);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        ParserTest_Utils.testRvalue(value, rvalue, instantiatedType);
    }

    /**
     * Type errors expect a NULL instantiated type on the lvalue. But the type of the rvalue still needs to be checked.
     */
    protected void testSimpleRightAssignment_TypeError(Statement statement, CoordinatePair statementLocation,
                                                     String identifier, VikariType declaredType,
                                                     VikariType instantiatedType, CoordinatePair lvalueLocation,
                                                     Object value) {

        assertEquals(statementLocation, statement.getLocation(), "Unexpected statement location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        Expression expression = ((ExpressionStatement) statement).getExpression();

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected statement type.");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        Expression lvalueExpression = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalueExpression.getClass(), "Unexpected lvalue type.");

        AtonementCrystal lvalue = ((VariableExpression) lvalueExpression).getReference();
        testVariableCrystal(lvalue, identifier, declaredType, VikariType.INVALID, lvalueLocation);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        ParserTest_Utils.testRvalue(value, rvalue, instantiatedType);
    }

    protected void testRightAssignment_FromVariable_TypeError(Statement statement, String lvalueIdentifier,
                                                            VikariType declaredType, CoordinatePair lvalueLocation,
                                                            String rvalueIdentifier, VikariType rvalueDeclaredType,
                                                            VikariType rvalueInstantiatedType,
                                                            CoordinatePair rvalueLocation) {

        assertEquals(rvalueLocation, statement.getLocation(), "Unexpected statement location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        Expression expression = ((ExpressionStatement) statement).getExpression();

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected statement type.");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        Expression lvalueExpression = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalueExpression.getClass(), "Unexpected lvalue type.");

        AtonementCrystal lvalue = ((VariableExpression) lvalueExpression).getReference();
        testVariableCrystal(lvalue, lvalueIdentifier, declaredType, VikariType.INVALID, lvalueLocation);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(VariableExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((VariableExpression) rvalueExpression).getReference();
        testVariableCrystal(rvalue, rvalueIdentifier, rvalueDeclaredType, rvalueInstantiatedType, rvalueLocation);
    }

    protected void testRightAssignment_NullKeyword(Statement statement, String identifier, VikariType declaredType,
                                                   CoordinatePair lvalueLocation, CoordinatePair rvalueLocation) {

        assertEquals(rvalueLocation, statement.getLocation(), "Unexpected location.");
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        Expression expression = expressionStatement.getExpression();
        assertEquals(rvalueLocation, expression.getLocation(), "Unexpected location.");

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected expression type");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        // test variable
        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, lvalueLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression rvalue = assignmentExpression.getRvalue();
        testNullKeyword(rvalue, rvalueLocation);
    }

    protected void testRightAssignment_NullSwordLiteral(Statement statement, String identifier, VikariType declaredType,
                                                        CoordinatePair lvalueLocation,
                                                        CoordinatePair rvalueLocation, int expectedLength) {

        assertEquals(rvalueLocation, statement.getLocation(), "Unexpected location.");
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        Expression expression = expressionStatement.getExpression();
        assertEquals(rvalueLocation, expression.getLocation(), "Unexpected location.");

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected expression type");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        // test variable
        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, lvalueLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression rvalue = assignmentExpression.getRvalue();
        testNullSwordLiteral(rvalue, rvalueLocation, expectedLength);
    }

    protected void testRightAssignment_NullLiteralExpression(Statement statement, String identifier,
                                                             VikariType declaredType, CoordinatePair lvalueLocation,
                                                             CoordinatePair rvalueLocation,
                                                             CoordinatePair operandLocation, int expectedLength) {

        assertEquals(rvalueLocation, statement.getLocation(), "Unexpected location.");
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        Expression expression = expressionStatement.getExpression();
        assertEquals(rvalueLocation, expression.getLocation(), "Unexpected location.");

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected expression type");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        // test variable
        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, identifier, declaredType, VikariType.NULL, lvalueLocation);

        // test operator
        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression rvalue = assignmentExpression.getRvalue();
        testNullLiteralExpression_SingleIntegerOperand(rvalue, rvalueLocation, operandLocation, expectedLength);
    }
}
