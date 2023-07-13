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
import com.atonementcrystals.dnr.vikari.error.VikariError;
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

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
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
    public void testParser_Expression_LeftAssignment_Basic() {
        List<Statement> statements = lexAndParse("foo,foo << 2");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleLeftAssignment(statement, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 4), 2);
    }

    @Test
    @Order(2)
    public void testParser_Expression_LeftAssignment_WithTypeLabel() {
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
        testSimpleLeftAssignment(statement, "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 13), 3);
    }

    @Test
    @Order(3)
    public void testParser_Expression_LeftAssignment_WithTypeLabel_ReAssignment() {
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
        testSimpleLeftAssignment(statement, "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 18), 4);
    }

    @Test
    @Order(4)
    public void testParser_Expression_LeftAssignment_MultipleVariables_SingleLine() {
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
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 15), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 25), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 35), 3);
    }

    @Test
    @Order(5)
    public void testParser_Expression_LeftAssignment_MultipleVariables_MultipleLines() {
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
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(3, 0), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(4, 0), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(5, 0), 3);
    }

    @Test
    @Order(6)
    public void testParser_Expression_LeftAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
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
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 39), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, location(0, 49), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, location(0, 59), 3);
    }

    @Test
    @Order(7)
    public void testParser_Expression_LeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
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
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 0), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 0), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 0), 3);
    }

    @Test
    @Order(8)
    public void testParser_Expression_LeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
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
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 0), 4);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 0), 5);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 0), 6);
    }

    @Test
    @Order(9)
    public void testParser_Expression_LeftAssignment_AllNumericTypes() {
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
        testSimpleLeftAssignment(statements.get(6), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 1);
        testSimpleLeftAssignment(statements.get(7), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, location(2, 0), 2L);
        testSimpleLeftAssignment(statements.get(8), "c", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_INTEGER, location(3, 0), new BigInteger("3"));
        testSimpleLeftAssignment(statements.get(9), "d", VikariType.ATONEMENT_CRYSTAL, VikariType.FLOAT, location(4, 0), 4.0F);
        testSimpleLeftAssignment(statements.get(10), "e", VikariType.ATONEMENT_CRYSTAL, VikariType.DOUBLE, location(5, 0), 5.0D);
        testSimpleLeftAssignment(statements.get(11), "f", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_DECIMAL, location(6, 0), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(10)
    public void testParser_Expression_LeftAssignment_AllNumericTypes_WithTypeLabels() {
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
        testSimpleLeftAssignment(statements.get(6), "a", VikariType.INTEGER, VikariType.INTEGER, location(6, 0), 1);
        testSimpleLeftAssignment(statements.get(7), "b", VikariType.LONG, VikariType.LONG, location(7, 0), 2L);
        testSimpleLeftAssignment(statements.get(8), "c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, location(8, 0), new BigInteger("3"));
        testSimpleLeftAssignment(statements.get(9), "d", VikariType.FLOAT, VikariType.FLOAT, location(9, 0), 4.0F);
        testSimpleLeftAssignment(statements.get(10), "e", VikariType.DOUBLE, VikariType.DOUBLE, location(10, 0), 5.0D);
        testSimpleLeftAssignment(statements.get(11), "f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, location(11, 0), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(11)
    public void testParser_Expression_LeftAssignment_NumericUpcasts() {
        String sourceString = """
                long1:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                float1:Float
                float2:Float
                float3:Float
                double1:Double
                double2:Double
                double3:Double
                double4:Double
                bigDecimal1:BigDecimal
                bigDecimal2:BigDecimal
                bigDecimal3:BigDecimal
                bigDecimal4:BigDecimal
                bigDecimal5:BigDecimal
                
                long1 << 1
                bigInteger1 << 2
                bigInteger2 << 3L
                float1 << 4
                float2 << 5L
                float3 << 6B
                double1 << 7
                double2 << 8L
                double3 << 9B
                double4 << 10.0F
                bigDecimal1 << 11
                bigDecimal2 << 12L
                bigDecimal3 << 13B
                bigDecimal4 << 14.0F
                bigDecimal5 << 15.0D
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }

        // assignment statements
        testSimpleLeftAssignment(statements.get(15), "long1", VikariType.LONG, VikariType.INTEGER, location(16, 0), 1);
        testSimpleLeftAssignment(statements.get(16), "bigInteger1", VikariType.BIG_INTEGER, VikariType.INTEGER, location(17, 0), 2);
        testSimpleLeftAssignment(statements.get(17), "bigInteger2", VikariType.BIG_INTEGER, VikariType.LONG, location(18, 0), 3L);
        testSimpleLeftAssignment(statements.get(18), "float1", VikariType.FLOAT, VikariType.INTEGER, location(19, 0), 4);
        testSimpleLeftAssignment(statements.get(19), "float2", VikariType.FLOAT, VikariType.LONG, location(20, 0), 5L);
        testSimpleLeftAssignment(statements.get(20), "float3", VikariType.FLOAT, VikariType.BIG_INTEGER, location(21, 0), new BigInteger("6"));
        testSimpleLeftAssignment(statements.get(21), "double1", VikariType.DOUBLE, VikariType.INTEGER, location(22, 0), 7);
        testSimpleLeftAssignment(statements.get(22), "double2", VikariType.DOUBLE, VikariType.LONG, location(23, 0), 8L);
        testSimpleLeftAssignment(statements.get(23), "double3", VikariType.DOUBLE, VikariType.BIG_INTEGER, location(24, 0), new BigInteger("9"));
        testSimpleLeftAssignment(statements.get(24), "double4", VikariType.DOUBLE, VikariType.FLOAT, location(25, 0), 10.0F);
        testSimpleLeftAssignment(statements.get(25), "bigDecimal1", VikariType.BIG_DECIMAL, VikariType.INTEGER, location(26, 0), 11);
        testSimpleLeftAssignment(statements.get(26), "bigDecimal2", VikariType.BIG_DECIMAL, VikariType.LONG, location(27, 0), 12L);
        testSimpleLeftAssignment(statements.get(27), "bigDecimal3", VikariType.BIG_DECIMAL, VikariType.BIG_INTEGER, location(28, 0), new BigInteger("13"));
        testSimpleLeftAssignment(statements.get(28), "bigDecimal4", VikariType.BIG_DECIMAL, VikariType.FLOAT, location(29, 0), 14.0F);
        testSimpleLeftAssignment(statements.get(29), "bigDecimal5", VikariType.BIG_DECIMAL, VikariType.DOUBLE, location(30, 0), 15.0D);
    }

    @Test
    @Order(12)
    public void testParser_Expression_LeftAssignment_NumericDowncasts() {
        String sourceString = """
                integer1:Integer
                integer2:Integer
                integer3:Integer
                integer4:Integer
                integer5:Integer
                long1:Long
                long2:Long
                long3:Long
                long4:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                bigInteger3:BigInteger
                float1:Float
                float2:Float
                double1:Double

                integer1 << 1L
                integer2 << 2B
                integer3 << 3.0F
                integer4 << 4.0D
                integer5 << 5.0B
                long1 << 6B
                long2 << 7.0F
                long3 << 8.0D
                long4 << 9.0B
                bigInteger1 << 10.0F
                bigInteger2 << 11.0D
                bigInteger3 << 12.0B
                float1 << 13.0D
                float2 << 14.0B
                double1 << 15.0B
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }

        MathContext mathContext = Arithmetic.getMathContext();

        // assignment statements
        testSimpleLeftAssignment(statements.get(15), "integer1", VikariType.INTEGER, VikariType.LONG, location(16, 0), 1L);
        testSimpleLeftAssignment(statements.get(16), "integer2", VikariType.INTEGER, VikariType.BIG_INTEGER, location(17, 0), new BigInteger("2"));
        testSimpleLeftAssignment(statements.get(17), "integer3", VikariType.INTEGER, VikariType.FLOAT, location(18, 0), 3.0F);
        testSimpleLeftAssignment(statements.get(18), "integer4", VikariType.INTEGER, VikariType.DOUBLE, location(19, 0), 4.0D);
        testSimpleLeftAssignment(statements.get(19), "integer5", VikariType.INTEGER, VikariType.BIG_DECIMAL, location(20, 0), new BigDecimal("5.0", mathContext));
        testSimpleLeftAssignment(statements.get(20), "long1", VikariType.LONG, VikariType.BIG_INTEGER, location(21, 0), new BigInteger("6"));
        testSimpleLeftAssignment(statements.get(21), "long2", VikariType.LONG, VikariType.FLOAT, location(22, 0), 7.0F);
        testSimpleLeftAssignment(statements.get(22), "long3", VikariType.LONG, VikariType.DOUBLE, location(23, 0), 8.0D);
        testSimpleLeftAssignment(statements.get(23), "long4", VikariType.LONG, VikariType.BIG_DECIMAL, location(24, 0), new BigDecimal("9.0", mathContext));
        testSimpleLeftAssignment(statements.get(24), "bigInteger1", VikariType.BIG_INTEGER, VikariType.FLOAT, location(25, 0), 10.0F);
        testSimpleLeftAssignment(statements.get(25), "bigInteger2", VikariType.BIG_INTEGER, VikariType.DOUBLE, location(26, 0), 11.0D);
        testSimpleLeftAssignment(statements.get(26), "bigInteger3", VikariType.BIG_INTEGER, VikariType.BIG_DECIMAL, location(27, 0), new BigDecimal("12.0", mathContext));
        testSimpleLeftAssignment(statements.get(27), "float1", VikariType.FLOAT, VikariType.DOUBLE, location(28, 0), 13.0D);
        testSimpleLeftAssignment(statements.get(28), "float2", VikariType.FLOAT, VikariType.BIG_DECIMAL, location(29, 0), new BigDecimal("14.0", mathContext));
        testSimpleLeftAssignment(statements.get(29), "double1", VikariType.DOUBLE, VikariType.BIG_DECIMAL, location(30, 0), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(13)
    public void testParser_Expression_LeftAssignment_AssignmentToParentTypes() {
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
        testSimpleLeftAssignment(statements.get(4), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 1);
        testSimpleLeftAssignment(statements.get(5), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(2, 0), 2);
        testSimpleLeftAssignment(statements.get(6), "c", VikariType.VALUE, VikariType.INTEGER, location(3, 0), 3);
        testSimpleLeftAssignment(statements.get(7), "d", VikariType.NUMBER, VikariType.INTEGER, location(4, 0), 4);
    }

    @Test
    @Order(14)
    public void testParser_Expression_LeftAssignment_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                foo << 2
                bar << 4
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar", "Unknown Type.");

        int expectedStatementCount = 4;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(2), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(2, 0), 2);
        testSimpleLeftAssignment(statements.get(3), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(3, 0), 4);
    }

    @Test
    @Order(15)
    public void testParser_Expression_LeftAssignment_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type, foo << 2";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 10), "foo:Type, foo << 2", "Variable " +
                "with type Type cannot be assigned a value of type Integer.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment_TypeError(statements.get(1), "foo", VikariType.TYPE, VikariType.INTEGER, location(0, 10), 2);
    }

    // --------------------------------
    // Right assignment operator tests.
    // --------------------------------

    @Test
    @Order(16)
    public void testParser_Expression_RightAssignment_Basic() {
        List<Statement> statements = lexAndParse("foo,2 >> foo");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleRightAssignment(statement, location(0, 4), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 9), 2);
    }

    @Test
    @Order(17)
    public void testParser_Expression_RightAssignment_WithTypeLabel() {
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
        testSimpleRightAssignment(statement, location(0, 13), "foo", VikariType.INTEGER, VikariType.INTEGER,
                location(0, 18), 3);
    }

    @Test
    @Order(18)
    public void testParser_Expression_RightAssignment_WithTypeLabel_ReAssignment() {
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
        testSimpleRightAssignment(statement, location(0, 18), "foo", VikariType.INTEGER, VikariType.INTEGER,
                location(0, 23), 4);
    }

    @Test
    @Order(19)
    public void testParser_Expression_RightAssignment_MultipleVariables_SingleLine() {
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
        testSimpleRightAssignment(statements.get(3), location(0, 15), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(0, 20), 1);

        testSimpleRightAssignment(statements.get(4), location(0, 25), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(0, 30), 2);

        testSimpleRightAssignment(statements.get(5), location(0, 35), "baz", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(0, 40), 3);
    }

    @Test
    @Order(20)
    public void testParser_Expression_RightAssignment_MultipleVariables_MultipleLines() {
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
        testSimpleRightAssignment(statements.get(3), location(3, 0), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(3, 5), 1);

        testSimpleRightAssignment(statements.get(4), location(4, 0), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(4, 5), 2);

        testSimpleRightAssignment(statements.get(5), location(5, 0), "baz", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(5, 5), 3);
    }

    @Test
    @Order(21)
    public void testParser_Expression_RightAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
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
        testSimpleRightAssignment(statements.get(3), location(0, 39), "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 44), 1);
        testSimpleRightAssignment(statements.get(4), location(0, 49) ,"bar", VikariType.INTEGER, VikariType.INTEGER, location(0, 54), 2);
        testSimpleRightAssignment(statements.get(5), location(0, 59), "baz", VikariType.INTEGER, VikariType.INTEGER, location(0, 64), 3);
    }

    @Test
    @Order(22)
    public void testParser_Expression_RightAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
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
        testSimpleRightAssignment(statements.get(3), location(3, 0), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 5), 1);
        testSimpleRightAssignment(statements.get(4), location(4, 0), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 5), 2);
        testSimpleRightAssignment(statements.get(5), location(5, 0), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 5), 3);
    }

    @Test
    @Order(23)
    public void testParser_Expression_RightAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
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
        testSimpleRightAssignment(statements.get(3), location(3, 0), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 5), 4);
        testSimpleRightAssignment(statements.get(4), location(4, 0), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 5), 5);
        testSimpleRightAssignment(statements.get(5), location(5, 0), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 5), 6);
    }

    @Test
    @Order(24)
    public void testParser_Expression_RightAssignment_AllNumericTypes() {
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
        testSimpleRightAssignment(statements.get(6), location(1, 0), "a", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(1, 5), 1);

        testSimpleRightAssignment(statements.get(7), location(2, 0), "b", VikariType.ATONEMENT_CRYSTAL,
                VikariType.LONG, location(2, 6), 2L);

        testSimpleRightAssignment(statements.get(8), location(3, 0), "c", VikariType.ATONEMENT_CRYSTAL,
                VikariType.BIG_INTEGER, location(3, 6), new BigInteger("3"));

        testSimpleRightAssignment(statements.get(9), location(4, 0), "d", VikariType.ATONEMENT_CRYSTAL,
                VikariType.FLOAT, location(4, 8), 4.0F);

        testSimpleRightAssignment(statements.get(10), location(5, 0), "e", VikariType.ATONEMENT_CRYSTAL,
                VikariType.DOUBLE, location(5, 8), 5.0D);

        testSimpleRightAssignment(statements.get(11), location(6, 0), "f", VikariType.ATONEMENT_CRYSTAL,
                VikariType.BIG_DECIMAL, location(6, 8), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(25)
    public void testParser_Expression_RightAssignment_AllNumericTypes_WithTypeLabels() {
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
        testSimpleRightAssignment(statements.get(6), location(6, 0), "a", VikariType.INTEGER,
                VikariType.INTEGER, location(6, 5), 1);

        testSimpleRightAssignment(statements.get(7), location(7, 0), "b", VikariType.LONG,
                VikariType.LONG, location(7, 6), 2L);

        testSimpleRightAssignment(statements.get(8), location(8, 0), "c", VikariType.BIG_INTEGER,
                VikariType.BIG_INTEGER, location(8, 6), new BigInteger("3"));

        testSimpleRightAssignment(statements.get(9), location(9, 0), "d", VikariType.FLOAT,
                VikariType.FLOAT, location(9, 8), 4.0F);

        testSimpleRightAssignment(statements.get(10), location(10, 0), "e", VikariType.DOUBLE,
                VikariType.DOUBLE, location(10, 8), 5.0D);

        testSimpleRightAssignment(statements.get(11), location(11, 0), "f", VikariType.BIG_DECIMAL,
                VikariType.BIG_DECIMAL, location(11, 8), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(26)
    public void testParser_Expression_RightAssignment_NumericUpcasts() {
        String sourceString = """
                long1:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                float1:Float
                float2:Float
                float3:Float
                double1:Double
                double2:Double
                double3:Double
                double4:Double
                bigDecimal1:BigDecimal
                bigDecimal2:BigDecimal
                bigDecimal3:BigDecimal
                bigDecimal4:BigDecimal
                bigDecimal5:BigDecimal

                1 >> long1
                2 >> bigInteger1
                3L >> bigInteger2
                4 >> float1
                5L >> float2
                6B >> float3
                7 >> double1
                8L >> double2
                9B >> double3
                10.0F >> double4
                11 >> bigDecimal1
                12L >> bigDecimal2
                13B >> bigDecimal3
                14.0F >> bigDecimal4
                15.0D >> bigDecimal5
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }
        // assignment statements
        testSimpleRightAssignment(statements.get(15), location(16, 0), "long1", VikariType.LONG,
                VikariType.INTEGER, location(16, 5), 1);

        testSimpleRightAssignment(statements.get(16), location(17, 0), "bigInteger1", VikariType.BIG_INTEGER,
                VikariType.INTEGER, location(17, 5), 2);

        testSimpleRightAssignment(statements.get(17), location(18, 0), "bigInteger2", VikariType.BIG_INTEGER,
                VikariType.LONG, location(18, 6), 3L);

        testSimpleRightAssignment(statements.get(18), location(19, 0), "float1", VikariType.FLOAT,
                VikariType.INTEGER, location(19, 5), 4);

        testSimpleRightAssignment(statements.get(19), location(20, 0), "float2", VikariType.FLOAT,
                VikariType.LONG, location(20, 6), 5L);

        testSimpleRightAssignment(statements.get(20), location(21, 0), "float3", VikariType.FLOAT,
                VikariType.BIG_INTEGER, location(21, 6), new BigInteger("6"));

        testSimpleRightAssignment(statements.get(21), location(22, 0), "double1", VikariType.DOUBLE,
                VikariType.INTEGER, location(22, 5), 7);

        testSimpleRightAssignment(statements.get(22), location(23, 0), "double2", VikariType.DOUBLE,
                VikariType.LONG, location(23, 6), 8L);

        testSimpleRightAssignment(statements.get(23), location(24, 0), "double3", VikariType.DOUBLE,
                VikariType.BIG_INTEGER, location(24, 6), new BigInteger("9"));

        testSimpleRightAssignment(statements.get(24), location(25, 0), "double4", VikariType.DOUBLE,
                VikariType.FLOAT, location(25, 9), 10.0F);

        testSimpleRightAssignment(statements.get(25), location(26, 0), "bigDecimal1", VikariType.BIG_DECIMAL,
                VikariType.INTEGER, location(26, 6), 11);

        testSimpleRightAssignment(statements.get(26), location(27, 0), "bigDecimal2", VikariType.BIG_DECIMAL,
                VikariType.LONG, location(27, 7), 12L);

        testSimpleRightAssignment(statements.get(27), location(28, 0), "bigDecimal3", VikariType.BIG_DECIMAL,
                VikariType.BIG_INTEGER, location(28, 7), new BigInteger("13"));

        testSimpleRightAssignment(statements.get(28), location(29, 0), "bigDecimal4", VikariType.BIG_DECIMAL,
                VikariType.FLOAT, location(29, 9), 14.0F);

        testSimpleRightAssignment(statements.get(29), location(30, 0), "bigDecimal5", VikariType.BIG_DECIMAL,
                VikariType.DOUBLE, location(30, 9), 15.0D);
    }

    @Test
    @Order(27)
    public void testParser_Expression_RightAssignment_NumericDowncasts() {
        String sourceString = """
                integer1:Integer
                integer2:Integer
                integer3:Integer
                integer4:Integer
                integer5:Integer
                long1:Long
                long2:Long
                long3:Long
                long4:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                bigInteger3:BigInteger
                float1:Float
                float2:Float
                double1:Double

                1L >> integer1
                2B >> integer2
                3.0F >> integer3
                4.0D >> integer4
                5.0B >> integer5
                6B >> long1
                7.0F >> long2
                8.0D >> long3
                9.0B >> long4
                10.0F >> bigInteger1
                11.0D >> bigInteger2
                12.0B >> bigInteger3
                13.0D >> float1
                14.0B >> float2
                15.0B >> double1
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }

        MathContext mathContext = Arithmetic.getMathContext();

        // assignment statements
        testSimpleRightAssignment(statements.get(15), location(16, 0), "integer1", VikariType.INTEGER,
                VikariType.LONG, location(16, 6), 1L);

        testSimpleRightAssignment(statements.get(16), location(17, 0), "integer2", VikariType.INTEGER,
                VikariType.BIG_INTEGER, location(17, 6), new BigInteger("2"));

        testSimpleRightAssignment(statements.get(17), location(18, 0), "integer3", VikariType.INTEGER,
                VikariType.FLOAT, location(18, 8), 3.0F);

        testSimpleRightAssignment(statements.get(18), location(19, 0), "integer4", VikariType.INTEGER,
                VikariType.DOUBLE, location(19, 8), 4.0D);

        testSimpleRightAssignment(statements.get(19), location(20, 0), "integer5", VikariType.INTEGER,
                VikariType.BIG_DECIMAL, location(20, 8), new BigDecimal("5.0", mathContext));

        testSimpleRightAssignment(statements.get(20), location(21, 0), "long1", VikariType.LONG,
                VikariType.BIG_INTEGER, location(21, 6), new BigInteger("6"));

        testSimpleRightAssignment(statements.get(21), location(22, 0), "long2", VikariType.LONG,
                VikariType.FLOAT, location(22, 8), 7.0F);

        testSimpleRightAssignment(statements.get(22), location(23, 0), "long3", VikariType.LONG,
                VikariType.DOUBLE, location(23, 8), 8.0D);

        testSimpleRightAssignment(statements.get(23), location(24, 0), "long4", VikariType.LONG,
                VikariType.BIG_DECIMAL, location(24, 8), new BigDecimal("9.0", mathContext));

        testSimpleRightAssignment(statements.get(24), location(25, 0), "bigInteger1", VikariType.BIG_INTEGER,
                VikariType.FLOAT, location(25, 9), 10.0F);

        testSimpleRightAssignment(statements.get(25), location(26, 0), "bigInteger2", VikariType.BIG_INTEGER,
                VikariType.DOUBLE, location(26, 9), 11.0D);

        testSimpleRightAssignment(statements.get(26), location(27, 0), "bigInteger3", VikariType.BIG_INTEGER,
                VikariType.BIG_DECIMAL, location(27, 9), new BigDecimal("12.0", mathContext));

        testSimpleRightAssignment(statements.get(27), location(28, 0), "float1", VikariType.FLOAT,
                VikariType.DOUBLE, location(28, 9), 13.0D);

        testSimpleRightAssignment(statements.get(28), location(29, 0), "float2", VikariType.FLOAT,
                VikariType.BIG_DECIMAL, location(29, 9), new BigDecimal("14.0", mathContext));

        testSimpleRightAssignment(statements.get(29), location(30, 0), "double1", VikariType.DOUBLE,
                VikariType.BIG_DECIMAL, location(30, 9), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(28)
    public void testParser_Expression_RightAssignment_AssignmentToParentTypes() {
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
        testSimpleRightAssignment(statements.get(4), location(1, 0), "a", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(1, 5), 1);

        testSimpleRightAssignment(statements.get(5), location(2, 0), "b", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(2, 5), 2);

        testSimpleRightAssignment(statements.get(6), location(3, 0), "c", VikariType.VALUE,
                VikariType.INTEGER, location(3, 5), 3);

        testSimpleRightAssignment(statements.get(7), location(4, 0), "d", VikariType.NUMBER,
                VikariType.INTEGER, location(4, 5), 4);
    }

    @Test
    @Order(29)
    public void testParser_Expression_RightAssignment_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                2 >> foo
                4 >> bar
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar", "Unknown Type.");

        int expectedStatementCount = 4;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(2), location(2, 0), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(2, 5), 2);

        testSimpleRightAssignment(statements.get(3), location(3, 0), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(3, 5), 4);
    }

    @Test
    @Order(30)
    public void testParser_Expression_RightAssignment_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type, 2 >> foo";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 15), "foo:Type, 2 >> foo", "Variable " +
                "with type Type cannot be assigned a value of type Integer.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment_TypeError(statements.get(1), location(0, 10), "foo", VikariType.TYPE,
                VikariType.INTEGER, location(0, 15), 2);
    }

    // -----------------
    // More error cases.
    // -----------------

    @Test
    @Order(31)
    public void testParser_Expression_Assignment_SyntaxError_InvalidAssignmentTarget() {
        String sourceString = "[5 + 2] << 7, 7 >> [5 + 2]";

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Invalid target for " +
                "assignment expression.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(0, 19), sourceString, "Invalid target for " +
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
