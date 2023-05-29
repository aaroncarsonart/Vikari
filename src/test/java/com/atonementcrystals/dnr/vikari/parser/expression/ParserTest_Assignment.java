package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.RightAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.interpreter.VikariProgram;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Leading variable declaration statements are necessary in order to test assignment expressions.
 * However, these declarations will not be explicitly tested in this test class, except for
 * asserting they have the correct statement type.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Assignment {
    private final AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();
    private AtonementField rootEnvironment;
    SyntaxErrorReporter syntaxErrorReporter;

    public List<Statement> lexAndParse(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        parser.setGlobalAtonementField(globalAtonementField);

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);
        rootEnvironment = parser.getRootEnvironment();

        return parsedStatements;
    }

    public List<Statement> lexAndParse_WithErrors(String sourceString, int expectedErrorCount) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        parser.setGlobalAtonementField(globalAtonementField);

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertSyntaxErrors(syntaxErrorReporter, expectedErrorCount);
        rootEnvironment = parser.getRootEnvironment();

        return parsedStatements;
    }

    public void testVariableCrystal(AtonementCrystal variable, String expectedIdentifier,
                                    VikariType expectedDeclaredType, VikariType expectedInstantiatedType,
                                    CoordinatePair expectedCoordinates) {

        testVariableCrystal(variable, expectedIdentifier, expectedDeclaredType, expectedInstantiatedType,
                expectedCoordinates, rootEnvironment);
    }

    public void testVariableCrystal(AtonementCrystal variable, String expectedIdentifier,
                                    VikariType expectedDeclaredType, VikariType expectedInstantiatedType,
                                    CoordinatePair expectedCoordinates, AtonementField localEnvironment) {
        // Check variable from expression
        assertEquals(expectedIdentifier, variable.getIdentifier(), "Unexpected variable identifier.");
        assertEquals(expectedDeclaredType.getTypeCrystal(), variable.getDeclaredType(), "Unexpected declared type.");

        if (expectedInstantiatedType == null) {
            assertNull(variable.getInstantiatedType(), "Expected instantiated type to be null.");
        } else{
            assertEquals(expectedInstantiatedType.getTypeCrystal(), variable.getInstantiatedType(), "Unexpected instantiated type.");
        }

        assertEquals(expectedCoordinates, variable.getCoordinates(), "Unexpected coordinates.");

        // Check variable exists in local environment
        if (localEnvironment != null) {
            assertTrue(localEnvironment.isDefined(expectedIdentifier), "Expected variable to be defined in local environment.");
            assertTrue(localEnvironment.hasFieldMember(expectedIdentifier), "Expected variable to be a field member of the local environment.");

            AtonementCrystal fieldMember = localEnvironment.get(expectedIdentifier);
            assertEquals(variable.getIdentifier(), fieldMember.getIdentifier(), "Expected equal identifiers.");
            assertEquals(variable.getDeclaredType(), fieldMember.getDeclaredType(), "Expected equal declared types.");
        }
    }

    private void testSimpleLeftAssignment(Statement statement, String identifier, VikariType declaredType,
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

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        TestUtils.testNumberCrystal(rvalue, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
    }

    /**
     * Type errors expect a null instantiated type on the lvalue. But the type of the rvalue still needs to be checked.
     */
    private void testSimpleLeftAssignment_TypeError(Statement statement, String identifier, VikariType declaredType,
                                                    VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(location, statement.getLocation(), "Unexpected statement location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        Expression expression = ((ExpressionStatement) statement).getExpression();

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected statement type.");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        Expression lvalueExpression = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalueExpression.getClass(), "Unexpected lvalue type.");

        AtonementCrystal lvalue = ((VariableExpression) lvalueExpression).getReference();
        testVariableCrystal(lvalue, identifier, declaredType, null, location);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        TestUtils.testNumberCrystal(rvalue, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
    }

    private void testSimpleRightAssignment(Statement statement, CoordinatePair statementLocation, String identifier,
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
        TestUtils.testNumberCrystal(rvalue, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
    }

    /**
     * Type errors expect a null instantiated type on the lvalue. But the type of the rvalue still needs to be checked.
     */
    private void testSimpleRightAssignment_TypeError(Statement statement, CoordinatePair statementLocation,
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
        testVariableCrystal(lvalue, identifier, declaredType, null, lvalueLocation);

        BinaryOperatorCrystal operator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = assignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        TestUtils.testNumberCrystal(rvalue, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
    }

    // -------------------------------
    // Left assignment operator tests.
    // -------------------------------

    @Test
    @Order(1)
    public void testLeftAssignment_Basic() {
        List<Statement> statements = lexAndParse("foo,foo << 2");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleLeftAssignment(statement, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(0, 4), 2);
    }

    @Test
    @Order(2)
    public void testLeftAssignment_WithTypeLabel() {
        String sourceString = "foo:Integer, foo << 3";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleLeftAssignment(statement, "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 13), 3);
    }

    @Test
    @Order(3)
    public void testLeftAssignment_WithTypeLabel_ReAssignment() {
        String sourceString = "foo:Integer << 2, foo << 4";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleLeftAssignment(statement, "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 18), 4);
    }

    @Test
    @Order(4)
    public void testLeftAssignment_MultipleVariables_SingleLine() {
        String sourceString = "foo, bar, baz, foo << 1, bar << 2, baz << 3";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(0, 15), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(0, 25), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(0, 35), 3);
    }

    @Test
    @Order(5)
    public void testLeftAssignment_MultipleVariables_MultipleLines() {
        String sourceString = """
                foo
                bar
                baz
                foo << 1
                bar << 2
                baz << 3
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(3, 0), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(4, 0), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(5, 0), 3);
    }

    @Test
    @Order(6)
    public void testLeftAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
        String sourceString = "foo:Integer, bar:Integer, baz:Integer, foo << 1, bar << 2, baz << 3";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 39), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 49), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 59), 3);
    }

    @Test
    @Order(7)
    public void testLeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
        String sourceString = """
                foo:Integer
                bar:Integer
                baz:Integer
                foo << 1
                bar << 2
                baz << 3
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(3, 0), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(4, 0), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(5, 0), 3);
    }

    @Test
    @Order(8)
    public void testLeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = """
                foo:Integer << 1
                bar:Integer << 2
                baz:Integer << 3
                foo << 4
                bar << 5
                baz << 6
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(3, 0), 4);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(4, 0), 5);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(5, 0), 6);
    }

    @Test
    @Order(9)
    public void testLeftAssignment_AllNumericTypes() {
        String sourceString = """
                a, b, c, d, e, f
                a << 1
                b << 2L
                c << 3B
                d << 4.0F
                e << 5.0D
                f << 6.0B
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(6), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(1, 0), 1);
        testSimpleLeftAssignment(statements.get(7), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, new CoordinatePair(2, 0), 2L);
        testSimpleLeftAssignment(statements.get(8), "c", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_INTEGER, new CoordinatePair(3, 0), new BigInteger("3"));
        testSimpleLeftAssignment(statements.get(9), "d", VikariType.ATONEMENT_CRYSTAL, VikariType.FLOAT, new CoordinatePair(4, 0), 4.0F);
        testSimpleLeftAssignment(statements.get(10), "e", VikariType.ATONEMENT_CRYSTAL, VikariType.DOUBLE, new CoordinatePair(5, 0), 5.0D);
        testSimpleLeftAssignment(statements.get(11), "f", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_DECIMAL, new CoordinatePair(6, 0), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(10)
    public void testLeftAssignment_AllNumericTypes_WithTypeLabels() {
        String sourceString = """
                a:Integer
                b:Long
                c:BigInteger
                d:Float
                e:Double
                f:BigDecimal
                a << 1
                b << 2L
                c << 3B
                d << 4.0F
                e << 5.0D
                f << 6.0B
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(6), "a", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(6, 0), 1);
        testSimpleLeftAssignment(statements.get(7), "b", VikariType.LONG, VikariType.LONG, new CoordinatePair(7, 0), 2L);
        testSimpleLeftAssignment(statements.get(8), "c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new CoordinatePair(8, 0), new BigInteger("3"));
        testSimpleLeftAssignment(statements.get(9), "d", VikariType.FLOAT, VikariType.FLOAT, new CoordinatePair(9, 0), 4.0F);
        testSimpleLeftAssignment(statements.get(10), "e", VikariType.DOUBLE, VikariType.DOUBLE, new CoordinatePair(10, 0), 5.0D);
        testSimpleLeftAssignment(statements.get(11), "f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new CoordinatePair(11, 0), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(11)
    public void testLeftAssignment_NumericUpcasts() {
        String sourceString = """
                long:Long
                bigInteger:BigInteger
                float:Float
                double:Double
                bigDecimal:BigDecimal
                
                long << 1
                bigInteger << 2
                bigInteger << 3L
                float << 4
                float << 5L
                float << 6B
                double << 7
                double << 8L
                double << 9B
                double << 10.0F
                bigDecimal << 11
                bigDecimal << 12L
                bigDecimal << 13B
                bigDecimal << 14.0F
                bigDecimal << 15.0D
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 20;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(5), "long", VikariType.LONG, VikariType.INTEGER, new CoordinatePair(6, 0), 1);
        testSimpleLeftAssignment(statements.get(6), "bigInteger", VikariType.BIG_INTEGER, VikariType.INTEGER, new CoordinatePair(7, 0), 2);
        testSimpleLeftAssignment(statements.get(7), "bigInteger", VikariType.BIG_INTEGER, VikariType.LONG, new CoordinatePair(8, 0), 3L);
        testSimpleLeftAssignment(statements.get(8), "float", VikariType.FLOAT, VikariType.INTEGER, new CoordinatePair(9, 0), 4);
        testSimpleLeftAssignment(statements.get(9), "float", VikariType.FLOAT, VikariType.LONG, new CoordinatePair(10, 0), 5L);
        testSimpleLeftAssignment(statements.get(10), "float", VikariType.FLOAT, VikariType.BIG_INTEGER, new CoordinatePair(11, 0), new BigInteger("6"));
        testSimpleLeftAssignment(statements.get(11), "double", VikariType.DOUBLE, VikariType.INTEGER, new CoordinatePair(12, 0), 7);
        testSimpleLeftAssignment(statements.get(12), "double", VikariType.DOUBLE, VikariType.LONG, new CoordinatePair(13, 0), 8L);
        testSimpleLeftAssignment(statements.get(13), "double", VikariType.DOUBLE, VikariType.BIG_INTEGER, new CoordinatePair(14, 0), new BigInteger("9"));
        testSimpleLeftAssignment(statements.get(14), "double", VikariType.DOUBLE, VikariType.FLOAT, new CoordinatePair(15, 0), 10.0F);
        testSimpleLeftAssignment(statements.get(15), "bigDecimal", VikariType.BIG_DECIMAL, VikariType.INTEGER, new CoordinatePair(16, 0), 11);
        testSimpleLeftAssignment(statements.get(16), "bigDecimal", VikariType.BIG_DECIMAL, VikariType.LONG, new CoordinatePair(17, 0), 12L);
        testSimpleLeftAssignment(statements.get(17), "bigDecimal", VikariType.BIG_DECIMAL, VikariType.BIG_INTEGER, new CoordinatePair(18, 0), new BigInteger("13"));
        testSimpleLeftAssignment(statements.get(18), "bigDecimal", VikariType.BIG_DECIMAL, VikariType.FLOAT, new CoordinatePair(19, 0), 14.0F);
        testSimpleLeftAssignment(statements.get(19), "bigDecimal", VikariType.BIG_DECIMAL, VikariType.DOUBLE, new CoordinatePair(20, 0), 15.0D);
    }

    @Test
    @Order(12)
    public void testLeftAssignment_NumericDowncasts() {
        String sourceString = """
                integer:Integer
                long:Long
                bigInteger:BigInteger
                float:Float
                double:Double

                integer << 1L
                integer << 2B
                integer << 3.0F
                integer << 4.0D
                integer << 5.0B
                long << 6B
                long << 7.0F
                long << 8.0D
                long << 9.0B
                bigInteger << 10.0F
                bigInteger << 11.0D
                bigInteger << 12.0B
                float << 13.0D
                float << 14.0B
                double << 15.0B
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 20;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");

        MathContext mathContext = Arithmetic.getMathContext();

        // assignment statements
        testSimpleLeftAssignment(statements.get(5), "integer", VikariType.INTEGER, VikariType.LONG, new CoordinatePair(6, 0), 1L);
        testSimpleLeftAssignment(statements.get(6), "integer", VikariType.INTEGER, VikariType.BIG_INTEGER, new CoordinatePair(7, 0), new BigInteger("2"));
        testSimpleLeftAssignment(statements.get(7), "integer", VikariType.INTEGER, VikariType.FLOAT, new CoordinatePair(8, 0), 3.0F);
        testSimpleLeftAssignment(statements.get(8), "integer", VikariType.INTEGER, VikariType.DOUBLE, new CoordinatePair(9, 0), 4.0D);
        testSimpleLeftAssignment(statements.get(9), "integer", VikariType.INTEGER, VikariType.BIG_DECIMAL, new CoordinatePair(10, 0), new BigDecimal("5.0", mathContext));
        testSimpleLeftAssignment(statements.get(10), "long", VikariType.LONG, VikariType.BIG_INTEGER, new CoordinatePair(11, 0), new BigInteger("6"));
        testSimpleLeftAssignment(statements.get(11), "long", VikariType.LONG, VikariType.FLOAT, new CoordinatePair(12, 0), 7.0F);
        testSimpleLeftAssignment(statements.get(12), "long", VikariType.LONG, VikariType.DOUBLE, new CoordinatePair(13, 0), 8.0D);
        testSimpleLeftAssignment(statements.get(13), "long", VikariType.LONG, VikariType.BIG_DECIMAL, new CoordinatePair(14, 0), new BigDecimal("9.0", mathContext));
        testSimpleLeftAssignment(statements.get(14), "bigInteger", VikariType.BIG_INTEGER, VikariType.FLOAT, new CoordinatePair(15, 0), 10.0F);
        testSimpleLeftAssignment(statements.get(15), "bigInteger", VikariType.BIG_INTEGER, VikariType.DOUBLE, new CoordinatePair(16, 0), 11.0D);
        testSimpleLeftAssignment(statements.get(16), "bigInteger", VikariType.BIG_INTEGER, VikariType.BIG_DECIMAL, new CoordinatePair(17, 0), new BigDecimal("12.0", mathContext));
        testSimpleLeftAssignment(statements.get(17), "float", VikariType.FLOAT, VikariType.DOUBLE, new CoordinatePair(18, 0), 13.0D);
        testSimpleLeftAssignment(statements.get(18), "float", VikariType.FLOAT, VikariType.BIG_DECIMAL, new CoordinatePair(19, 0), new BigDecimal("14.0", mathContext));
        testSimpleLeftAssignment(statements.get(19), "double", VikariType.DOUBLE, VikariType.BIG_DECIMAL, new CoordinatePair(20, 0), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(13)
    public void testLeftAssignment_AssignmentToParentTypes() {
        String sourceString = """
                a, b:AtonementCrystal, c:Value, d:Number
                a << 1
                b << 2
                c << 3
                d << 4
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 8;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(4), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(1, 0), 1);
        testSimpleLeftAssignment(statements.get(5), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(2, 0), 2);
        testSimpleLeftAssignment(statements.get(6), "c", VikariType.VALUE, VikariType.INTEGER, new CoordinatePair(3, 0), 3);
        testSimpleLeftAssignment(statements.get(7), "d", VikariType.NUMBER, VikariType.INTEGER, new CoordinatePair(4, 0), 4);
    }

    @Test
    @Order(14)
    public void testLeftAssignment_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                foo << 2
                bar << 4
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 4), "foo:Foo", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), new CoordinatePair(1, 4), "bar:Bar", "Unknown Type.");

        int expectedStatementCount = 4;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(2), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(2, 0), 2);
        testSimpleLeftAssignment(statements.get(3), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(3, 0), 4);
    }

    @Test
    @Order(15)
    public void testLeftAssignment_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type, foo << 2";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 10), "foo:Type, foo << 2", "Variable " +
                "with type Type cannot be assigned a value of type Integer.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment_TypeError(statements.get(1), "foo", VikariType.TYPE, VikariType.INTEGER, new CoordinatePair(0, 10), 2);
    }

    // --------------------------------
    // Right assignment operator tests.
    // --------------------------------

    @Test
    @Order(16)
    public void testRightAssignment_Basic() {
        List<Statement> statements = lexAndParse("foo,2 >> foo");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleRightAssignment(statement, new CoordinatePair(0, 4), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, new CoordinatePair(0, 9), 2);
    }

    @Test
    @Order(17)
    public void testRightAssignment_WithTypeLabel() {
        String sourceString = "foo:Integer, 3 >> foo";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleRightAssignment(statement, new CoordinatePair(0, 13), "foo", VikariType.INTEGER, VikariType.INTEGER,
                new CoordinatePair(0, 18), 3);
    }

    @Test
    @Order(18)
    public void testRightAssignment_WithTypeLabel_ReAssignment() {
        String sourceString = "foo:Integer << 2, 4 >> foo";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleRightAssignment(statement, new CoordinatePair(0, 18), "foo", VikariType.INTEGER, VikariType.INTEGER,
                new CoordinatePair(0, 23), 4);
    }

    @Test
    @Order(19)
    public void testRightAssignment_MultipleVariables_SingleLine() {
        String sourceString = "foo, bar, baz, 1 >> foo, 2 >> bar, 3 >> baz";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), new CoordinatePair(0, 15), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(0, 20), 1);

        testSimpleRightAssignment(statements.get(4), new CoordinatePair(0, 25), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(0, 30), 2);

        testSimpleRightAssignment(statements.get(5), new CoordinatePair(0, 35), "baz", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(0, 40), 3);
    }

    @Test
    @Order(20)
    public void testRightAssignment_MultipleVariables_MultipleLines() {
        String sourceString = """
                foo
                bar
                baz
                1 >> foo
                2 >> bar
                3 >> baz
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), new CoordinatePair(3, 0), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(3, 5), 1);

        testSimpleRightAssignment(statements.get(4), new CoordinatePair(4, 0), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(4, 5), 2);

        testSimpleRightAssignment(statements.get(5), new CoordinatePair(5, 0), "baz", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(5, 5), 3);
    }

    @Test
    @Order(21)
    public void testRightAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
        String sourceString = "foo:Integer, bar:Integer, baz:Integer, 1 >> foo, 2 >> bar, 3 >> baz";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), new CoordinatePair(0, 39), "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 44), 1);
        testSimpleRightAssignment(statements.get(4), new CoordinatePair(0, 49) ,"bar", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 54), 2);
        testSimpleRightAssignment(statements.get(5), new CoordinatePair(0, 59), "baz", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(0, 64), 3);
    }

    @Test
    @Order(22)
    public void testRightAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
        String sourceString = """
                foo:Integer
                bar:Integer
                baz:Integer
                1 >> foo
                2 >> bar
                3 >> baz
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), new CoordinatePair(3, 0), "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(3, 5), 1);
        testSimpleRightAssignment(statements.get(4), new CoordinatePair(4, 0), "bar", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(4, 5), 2);
        testSimpleRightAssignment(statements.get(5), new CoordinatePair(5, 0), "baz", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(5, 5), 3);
    }

    @Test
    @Order(23)
    public void testRightAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = """
                foo:Integer << 1
                bar:Integer << 2
                baz:Integer << 3
                4 >> foo
                5 >> bar
                6 >> baz
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), new CoordinatePair(3, 0), "foo", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(3, 5), 4);
        testSimpleRightAssignment(statements.get(4), new CoordinatePair(4, 0), "bar", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(4, 5), 5);
        testSimpleRightAssignment(statements.get(5), new CoordinatePair(5, 0), "baz", VikariType.INTEGER, VikariType.INTEGER, new CoordinatePair(5, 5), 6);
    }

    @Test
    @Order(24)
    public void testRightAssignment_AllNumericTypes() {
        String sourceString = """
                a, b, c, d, e, f
                1 >> a
                2L >> b
                3B >> c
                4.0F >> d
                5.0D >> e
                6.0B >> f
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(6), new CoordinatePair(1, 0), "a", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(1, 5), 1);

        testSimpleRightAssignment(statements.get(7), new CoordinatePair(2, 0), "b", VikariType.ATONEMENT_CRYSTAL,
                VikariType.LONG, new CoordinatePair(2, 6), 2L);

        testSimpleRightAssignment(statements.get(8), new CoordinatePair(3, 0), "c", VikariType.ATONEMENT_CRYSTAL,
                VikariType.BIG_INTEGER, new CoordinatePair(3, 6), new BigInteger("3"));

        testSimpleRightAssignment(statements.get(9), new CoordinatePair(4, 0), "d", VikariType.ATONEMENT_CRYSTAL,
                VikariType.FLOAT, new CoordinatePair(4, 8), 4.0F);

        testSimpleRightAssignment(statements.get(10), new CoordinatePair(5, 0), "e", VikariType.ATONEMENT_CRYSTAL,
                VikariType.DOUBLE, new CoordinatePair(5, 8), 5.0D);

        testSimpleRightAssignment(statements.get(11), new CoordinatePair(6, 0), "f", VikariType.ATONEMENT_CRYSTAL,
                VikariType.BIG_DECIMAL, new CoordinatePair(6, 8), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(25)
    public void testRightAssignment_AllNumericTypes_WithTypeLabels() {
        String sourceString = """
                a:Integer
                b:Long
                c:BigInteger
                d:Float
                e:Double
                f:BigDecimal
                1 >> a
                2L >> b
                3B >> c
                4.0F >> d
                5.0D >> e
                6.0B >> f
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(6), new CoordinatePair(6, 0), "a", VikariType.INTEGER,
                VikariType.INTEGER, new CoordinatePair(6, 5), 1);

        testSimpleRightAssignment(statements.get(7), new CoordinatePair(7, 0), "b", VikariType.LONG,
                VikariType.LONG, new CoordinatePair(7, 6), 2L);

        testSimpleRightAssignment(statements.get(8), new CoordinatePair(8, 0), "c", VikariType.BIG_INTEGER,
                VikariType.BIG_INTEGER, new CoordinatePair(8, 6), new BigInteger("3"));

        testSimpleRightAssignment(statements.get(9), new CoordinatePair(9, 0), "d", VikariType.FLOAT,
                VikariType.FLOAT, new CoordinatePair(9, 8), 4.0F);

        testSimpleRightAssignment(statements.get(10), new CoordinatePair(10, 0), "e", VikariType.DOUBLE,
                VikariType.DOUBLE, new CoordinatePair(10, 8), 5.0D);

        testSimpleRightAssignment(statements.get(11), new CoordinatePair(11, 0), "f", VikariType.BIG_DECIMAL,
                VikariType.BIG_DECIMAL, new CoordinatePair(11, 8), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(26)
    public void testRightAssignment_NumericUpcasts() {
        String sourceString = """
                long:Long
                bigInteger:BigInteger
                float:Float
                double:Double
                bigDecimal:BigDecimal

                1 >> long
                2 >> bigInteger
                3L >> bigInteger
                4 >> float
                5L >> float
                6B >> float
                7 >> double
                8L >> double
                9B >> double
                10.0F >> double
                11 >> bigDecimal
                12L >> bigDecimal
                13B >> bigDecimal
                14.0F >> bigDecimal
                15.0D >> bigDecimal
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 20;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(5), new CoordinatePair(6, 0), "long", VikariType.LONG,
                VikariType.INTEGER, new CoordinatePair(6, 5), 1);

        testSimpleRightAssignment(statements.get(6), new CoordinatePair(7, 0), "bigInteger", VikariType.BIG_INTEGER,
                VikariType.INTEGER, new CoordinatePair(7, 5), 2);

        testSimpleRightAssignment(statements.get(7), new CoordinatePair(8, 0), "bigInteger", VikariType.BIG_INTEGER,
                VikariType.LONG, new CoordinatePair(8, 6), 3L);

        testSimpleRightAssignment(statements.get(8), new CoordinatePair(9, 0), "float", VikariType.FLOAT,
                VikariType.INTEGER, new CoordinatePair(9, 5), 4);

        testSimpleRightAssignment(statements.get(9), new CoordinatePair(10, 0), "float", VikariType.FLOAT,
                VikariType.LONG, new CoordinatePair(10, 6), 5L);

        testSimpleRightAssignment(statements.get(10), new CoordinatePair(11, 0), "float", VikariType.FLOAT,
                VikariType.BIG_INTEGER, new CoordinatePair(11, 6), new BigInteger("6"));

        testSimpleRightAssignment(statements.get(11), new CoordinatePair(12, 0), "double", VikariType.DOUBLE,
                VikariType.INTEGER, new CoordinatePair(12, 5), 7);

        testSimpleRightAssignment(statements.get(12), new CoordinatePair(13, 0), "double", VikariType.DOUBLE,
                VikariType.LONG, new CoordinatePair(13, 6), 8L);

        testSimpleRightAssignment(statements.get(13), new CoordinatePair(14, 0), "double", VikariType.DOUBLE,
                VikariType.BIG_INTEGER, new CoordinatePair(14, 6), new BigInteger("9"));

        testSimpleRightAssignment(statements.get(14), new CoordinatePair(15, 0), "double", VikariType.DOUBLE,
                VikariType.FLOAT, new CoordinatePair(15, 9), 10.0F);

        testSimpleRightAssignment(statements.get(15), new CoordinatePair(16, 0), "bigDecimal", VikariType.BIG_DECIMAL,
                VikariType.INTEGER, new CoordinatePair(16, 6), 11);

        testSimpleRightAssignment(statements.get(16), new CoordinatePair(17, 0), "bigDecimal", VikariType.BIG_DECIMAL,
                VikariType.LONG, new CoordinatePair(17, 7), 12L);

        testSimpleRightAssignment(statements.get(17), new CoordinatePair(18, 0), "bigDecimal", VikariType.BIG_DECIMAL,
                VikariType.BIG_INTEGER, new CoordinatePair(18, 7), new BigInteger("13"));

        testSimpleRightAssignment(statements.get(18), new CoordinatePair(19, 0), "bigDecimal", VikariType.BIG_DECIMAL,
                VikariType.FLOAT, new CoordinatePair(19, 9), 14.0F);

        testSimpleRightAssignment(statements.get(19), new CoordinatePair(20, 0), "bigDecimal", VikariType.BIG_DECIMAL,
                VikariType.DOUBLE, new CoordinatePair(20, 9), 15.0D);
    }

    @Test
    @Order(27)
    public void testRightAssignment_NumericDowncasts() {
        String sourceString = """
                integer:Integer
                long:Long
                bigInteger:BigInteger
                float:Float
                double:Double

                1L >> integer
                2B >> integer
                3.0F >> integer
                4.0D >> integer
                5.0B >> integer
                6B >> long
                7.0F >> long
                8.0D >> long
                9.0B >> long
                10.0F >> bigInteger
                11.0D >> bigInteger
                12.0B >> bigInteger
                13.0D >> float
                14.0B >> float
                15.0B >> double
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 20;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");

        MathContext mathContext = Arithmetic.getMathContext();

        // assignment statements
        testSimpleRightAssignment(statements.get(5), new CoordinatePair(6, 0), "integer", VikariType.INTEGER,
                VikariType.LONG, new CoordinatePair(6, 6), 1L);

        testSimpleRightAssignment(statements.get(6), new CoordinatePair(7, 0), "integer", VikariType.INTEGER,
                VikariType.BIG_INTEGER, new CoordinatePair(7, 6), new BigInteger("2"));

        testSimpleRightAssignment(statements.get(7), new CoordinatePair(8, 0), "integer", VikariType.INTEGER,
                VikariType.FLOAT, new CoordinatePair(8, 8), 3.0F);

        testSimpleRightAssignment(statements.get(8), new CoordinatePair(9, 0), "integer", VikariType.INTEGER,
                VikariType.DOUBLE, new CoordinatePair(9, 8), 4.0D);

        testSimpleRightAssignment(statements.get(9), new CoordinatePair(10, 0), "integer", VikariType.INTEGER,
                VikariType.BIG_DECIMAL, new CoordinatePair(10, 8), new BigDecimal("5.0", mathContext));

        testSimpleRightAssignment(statements.get(10), new CoordinatePair(11, 0), "long", VikariType.LONG,
                VikariType.BIG_INTEGER, new CoordinatePair(11, 6), new BigInteger("6"));

        testSimpleRightAssignment(statements.get(11), new CoordinatePair(12, 0), "long", VikariType.LONG,
                VikariType.FLOAT, new CoordinatePair(12, 8), 7.0F);

        testSimpleRightAssignment(statements.get(12), new CoordinatePair(13, 0), "long", VikariType.LONG,
                VikariType.DOUBLE, new CoordinatePair(13, 8), 8.0D);

        testSimpleRightAssignment(statements.get(13), new CoordinatePair(14, 0), "long", VikariType.LONG,
                VikariType.BIG_DECIMAL, new CoordinatePair(14, 8), new BigDecimal("9.0", mathContext));

        testSimpleRightAssignment(statements.get(14), new CoordinatePair(15, 0), "bigInteger", VikariType.BIG_INTEGER,
                VikariType.FLOAT, new CoordinatePair(15, 9), 10.0F);

        testSimpleRightAssignment(statements.get(15), new CoordinatePair(16, 0), "bigInteger", VikariType.BIG_INTEGER,
                VikariType.DOUBLE, new CoordinatePair(16, 9), 11.0D);

        testSimpleRightAssignment(statements.get(16), new CoordinatePair(17, 0), "bigInteger", VikariType.BIG_INTEGER,
                VikariType.BIG_DECIMAL, new CoordinatePair(17, 9), new BigDecimal("12.0", mathContext));

        testSimpleRightAssignment(statements.get(17), new CoordinatePair(18, 0), "float", VikariType.FLOAT,
                VikariType.DOUBLE, new CoordinatePair(18, 9), 13.0D);

        testSimpleRightAssignment(statements.get(18), new CoordinatePair(19, 0), "float", VikariType.FLOAT,
                VikariType.BIG_DECIMAL, new CoordinatePair(19, 9), new BigDecimal("14.0", mathContext));

        testSimpleRightAssignment(statements.get(19), new CoordinatePair(20, 0), "double", VikariType.DOUBLE,
                VikariType.BIG_DECIMAL, new CoordinatePair(20, 9), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(28)
    public void testRightAssignment_AssignmentToParentTypes() {
        String sourceString = """
                a, b:AtonementCrystal, c:Value, d:Number
                1 >> a
                2 >> b
                3 >> c
                4 >> d
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 8;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(4), new CoordinatePair(1, 0), "a", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(1, 5), 1);

        testSimpleRightAssignment(statements.get(5), new CoordinatePair(2, 0), "b", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(2, 5), 2);

        testSimpleRightAssignment(statements.get(6), new CoordinatePair(3, 0), "c", VikariType.VALUE,
                VikariType.INTEGER, new CoordinatePair(3, 5), 3);

        testSimpleRightAssignment(statements.get(7), new CoordinatePair(4, 0), "d", VikariType.NUMBER,
                VikariType.INTEGER, new CoordinatePair(4, 5), 4);
    }

    @Test
    @Order(29)
    public void testRightAssignment_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                2 >> foo
                4 >> bar
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 4), "foo:Foo", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), new CoordinatePair(1, 4), "bar:Bar", "Unknown Type.");

        int expectedStatementCount = 4;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(2), new CoordinatePair(2, 0), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(2, 5), 2);

        testSimpleRightAssignment(statements.get(3), new CoordinatePair(3, 0), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, new CoordinatePair(3, 5), 4);
    }

    @Test
    @Order(30)
    public void testRightAssignment_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type, 2 >> foo";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 15), "foo:Type, 2 >> foo", "Variable " +
                "with type Type cannot be assigned a value of type Integer.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment_TypeError(statements.get(1), new CoordinatePair(0, 10), "foo", VikariType.TYPE,
                VikariType.INTEGER, new CoordinatePair(0, 15), 2);
    }

    // -----------------
    // More error cases.
    // -----------------

    @Test
    @Order(31)
    public void testAssignment_SyntaxError_InvalidAssignmentTarget() {
        String sourceString = "[5 + 2] << 7, 7 >> [5 + 2]";

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 0), sourceString, "Invalid target for " +
                "assignment expression.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), new CoordinatePair(0, 19), sourceString, "Invalid target for " +
                "assignment expression.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // left assignment statement: "[5 + 2] << 7"
        Statement statement = statements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression innerExpression = expressionStatement.getExpression();
        assertEquals(LeftAssignmentExpression.class, innerExpression.getClass(), "Unexpected expression type.");
        LeftAssignmentExpression leftAssignmentExpression = (LeftAssignmentExpression) innerExpression;

        // erroneous lvalue (no need to assert further.)
        Expression lvalueExpression = leftAssignmentExpression.getLvalue();
        assertEquals(GroupingExpression.class, lvalueExpression.getClass(), "Unexpected expression type.");

        BinaryOperatorCrystal operator = leftAssignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = leftAssignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        TestUtils.testNumberCrystal(rvalue, 7, IntegerCrystal.class);

        // right assignment statement: "7 >> [5 + 2]"
        statement = statements.get(1);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        expressionStatement = (ExpressionStatement) statement;
        innerExpression = expressionStatement.getExpression();
        assertEquals(RightAssignmentExpression.class, innerExpression.getClass(), "Unexpected expression type.");
        RightAssignmentExpression rightAssignmentExpression = (RightAssignmentExpression) innerExpression;

        // erroneous lvalue (no need to assert further.)
        lvalueExpression = rightAssignmentExpression.getLvalue();
        assertEquals(GroupingExpression.class, lvalueExpression.getClass(), "Unexpected expression type.");

        operator = rightAssignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        rvalueExpression = rightAssignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        rvalue = ((LiteralExpression) rvalueExpression).getValue();
        TestUtils.testNumberCrystal(rvalue, 7, IntegerCrystal.class);
    }
}
