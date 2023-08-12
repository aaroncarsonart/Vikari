package com.atonementcrystals.dnr.vikari.parser;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullKeywordCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.BooleanLogicExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.NullLiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Helper methods for Parser tests.
 */
public class ParserTest_Utils {
    public static List<Statement> lexAndParse(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);
        return parsedStatements;
    }

    public static List<Statement> lexAndParse(String sourceString, SyntaxErrorReporter syntaxErrorReporter,
                                              int expectedErrorCount) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertSyntaxErrors(syntaxErrorReporter, expectedErrorCount);
        return parsedStatements;
    }

    private static void testTypeCrystalsOfLiteral(AtonementCrystal literalCrystal, VikariType vikariType) {
        TypeCrystal expectedType = vikariType.getTypeCrystal();

        TypeCrystal declaredType = literalCrystal.getDeclaredType();
        assertEquals(declaredType, expectedType, "Unexpected declared type.");

        TypeCrystal instantiatedType = literalCrystal.getInstantiatedType();
        assertEquals(instantiatedType, expectedType, "Unexpected instantiated type.");
    }

    private static void testNumberCrystalLiteralExpression(Expression expr, Object expectedValue,
                                                           Class<? extends NumberCrystal<?>> clazz) {
        assertEquals(LiteralExpression.class, expr.getClass(), "Unexpected expression type.");

        LiteralExpression literalExpression = (LiteralExpression) expr;
        AtonementCrystal value = literalExpression.getValue();
        assertEquals(clazz, value.getClass(), "Unexpected literal type.");

        NumberCrystal<?> number = clazz.cast(value);
        assertEquals(expectedValue, number.getValue(), "Unexpected literal value.");
    }

    public static void testIntegerLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, IntegerCrystal.class);

        AtonementCrystal literalCrystal = ((LiteralExpression) expr).getValue();
        testTypeCrystalsOfLiteral(literalCrystal, VikariType.INTEGER);
    }

    public static void testLongLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, LongCrystal.class);

        AtonementCrystal literalCrystal = ((LiteralExpression) expr).getValue();
        testTypeCrystalsOfLiteral(literalCrystal, VikariType.LONG);
    }

    public static void testBigIntegerLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, BigIntegerCrystal.class);

        AtonementCrystal literalCrystal = ((LiteralExpression) expr).getValue();
        testTypeCrystalsOfLiteral(literalCrystal, VikariType.BIG_INTEGER);
    }

    public static void testFloatLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, FloatCrystal.class);

        AtonementCrystal literalCrystal = ((LiteralExpression) expr).getValue();
        testTypeCrystalsOfLiteral(literalCrystal, VikariType.FLOAT);
    }

    public static void testDoubleLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, DoubleCrystal.class);

        AtonementCrystal literalCrystal = ((LiteralExpression) expr).getValue();
        testTypeCrystalsOfLiteral(literalCrystal, VikariType.DOUBLE);
    }

    public static void testBigDecimalLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, BigDecimalCrystal.class);

        AtonementCrystal literalCrystal = ((LiteralExpression) expr).getValue();
        testTypeCrystalsOfLiteral(literalCrystal, VikariType.BIG_DECIMAL);
    }

    public static void testBinaryExpression(Expression expr,
                                            Class<? extends BinaryOperatorCrystal> expectedOperatorClass,
                                            int expectedLeft, int expectedRight) {
        assertEquals(BinaryExpression.class, expr.getClass(), "Unexpected expression type.");
        BinaryExpression binaryExpression = (BinaryExpression) expr;

        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(expectedOperatorClass, operator.getClass(), "Unexpected operator type.");
        testIntegerLiteralExpression(left, expectedLeft);
        testIntegerLiteralExpression(right, expectedRight);
    }

    public static void testRvalue(Object value, AtonementCrystal rvalue, VikariType instantiatedType) {
        if (value instanceof Number) {
            TestUtils.testNumberCrystal(rvalue, value, (Class<? extends NumberCrystal<?>>) instantiatedType.getJavaType());
        } else if (value instanceof Boolean) {
            TestUtils.testBooleanCrystal(rvalue, value);
        } else {
            fail("Malformed test. Unexpected value type for rvalue: " + value.getClass().getSimpleName());
        }
    }

    public static void testNullKeyword(Expression expression, CoordinatePair expectedLocation) {
        assertEquals(expectedLocation, expression.getLocation(), "Unexpected location.");
        assertEquals(LiteralExpression.class, expression.getClass(), "Unexpected expression type.");

        LiteralExpression literalExpression = (LiteralExpression) expression;
        AtonementCrystal literal = literalExpression.getValue();

        assertEquals(expectedLocation, literal.getCoordinates(), "Unexpected location.");
        assertEquals(NullKeywordCrystal.class, literal.getClass(), "Unexpected crystal type.");

        NullKeywordCrystal nullKeywordCrystal = (NullKeywordCrystal) literal;
        assertEquals(0, nullKeywordCrystal.getLength(), "Unexpected length.");
    }

    public static void testNullSwordLiteral(Expression expression, CoordinatePair expectedLocation,
                                            int expectedLength) {

        assertEquals(expectedLocation, expression.getLocation(), "Unexpected location.");
        assertEquals(LiteralExpression.class, expression.getClass(), "Unexpected expression type.");

        LiteralExpression literalExpression = (LiteralExpression) expression;
        AtonementCrystal literal = literalExpression.getValue();

        assertEquals(expectedLocation, literal.getCoordinates(), "Unexpected location.");
        assertEquals(NullCrystal.class, literal.getClass(), "Unexpected crystal type.");

        NullCrystal nullCrystal = (NullCrystal) literal;
        assertEquals(expectedLength, nullCrystal.getLength(), "Unexpected length.");
    }

    public static void testNullLiteralExpression_SingleIntegerOperand(Expression expression,
                                                                      CoordinatePair expectedExpressionLocation,
                                                                      CoordinatePair expectedOperandLocation,
                                                                      int expectedLength) {

        testNullLiteralExpression_SingleNumberOperand(expression, expectedExpressionLocation, expectedOperandLocation,
                IntegerCrystal.class, expectedLength);
    }

    public static void testNullLiteralExpression_SingleNumberOperand(Expression expression,
                                                                     CoordinatePair expectedExpressionLocation,
                                                                     CoordinatePair expectedOperandLocation,
                                                                     Class<? extends NumberCrystal<? extends Number>> numberType,
                                                                     Number expectedLength) {

        assertEquals(expectedExpressionLocation, expression.getLocation(), "Unexpected location.");
        assertEquals(NullLiteralExpression.class, expression.getClass(), "Unexpected expression type.");

        NullLiteralExpression nullLiteralExpression = (NullLiteralExpression) expression;
        Expression innerExpression = nullLiteralExpression.getExpression();
        assertEquals(LiteralExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        LiteralExpression literalExpression = (LiteralExpression) innerExpression;
        AtonementCrystal literal = literalExpression.getValue();

        assertEquals(expectedOperandLocation, literal.getCoordinates(), "Unexpected location.");
        testNumberCrystal(literal, expectedLength, numberType);
    }

    public static void assertStatementCount(List<Statement> statements, int expectedCount) {
        int actualCount = statements.size();
        assertEquals(expectedCount, actualCount, "Unexpected statement count.");
    }

    public static ExpressionStatement assertExpressionStatement(Statement statement, CoordinatePair expectedLocation) {
        assertLocation(statement, expectedLocation);
        return assertType(statement, ExpressionStatement.class);
    }

    public static BooleanLogicExpression assertLogicalExpression(Expression expression, CoordinatePair expectedLocation) {
        assertLocation(expression, expectedLocation);
        return assertType(expression, BooleanLogicExpression.class);
    }

    public static BooleanLogicExpression assertLogicalExpression(Statement statement, CoordinatePair expectedLocation) {
        ExpressionStatement expressionStatement = assertExpressionStatement(statement, expectedLocation);
        return assertLogicalExpression(expressionStatement.getExpression(), expectedLocation);
    }

    public static UnaryExpression assertUnaryExpression(Expression expression, CoordinatePair expectedLocation) {
        assertLocation(expression, expectedLocation);
        return assertType(expression, UnaryExpression.class);
    }

    public static GroupingExpression assertGroupingExpression(Statement statement, CoordinatePair expectedLocation) {
        ExpressionStatement expressionStatement = assertExpressionStatement(statement, expectedLocation);
        return assertType(expressionStatement.getExpression(), GroupingExpression.class);
    }

    public static GroupingExpression assertGroupingExpression(Expression expression, CoordinatePair expectedLocation) {
        assertLocation(expression, expectedLocation);
        return assertType(expression, GroupingExpression.class);
    }

    public static VariableDeclarationStatement assertVariableDeclaration(Statement statement, CoordinatePair expectedLocation) {
        assertLocation(statement, expectedLocation);
        return assertType(statement, VariableDeclarationStatement.class);
    }

    public static LeftAssignmentExpression assertLeftAssignment(Statement statement, CoordinatePair expectedLocation) {
        ExpressionStatement expressionStatement = assertExpressionStatement(statement, expectedLocation);
        return assertLeftAssignment(expressionStatement.getExpression(), expectedLocation);
    }

    public static LeftAssignmentExpression assertLeftAssignment(Expression expression, CoordinatePair expectedLocation) {
        assertLocation(expression, expectedLocation);
        return assertType(expression, LeftAssignmentExpression.class);
    }

    public static RightAssignmentExpression assertRightAssignment(Statement statement, CoordinatePair expectedLocation) {
        ExpressionStatement expressionStatement = assertExpressionStatement(statement, expectedLocation);
        return assertRightAssignment(expressionStatement.getExpression(), expectedLocation);
    }

    public static RightAssignmentExpression assertRightAssignment(Expression expression, CoordinatePair expectedLocation) {
        assertLocation(expression, expectedLocation);
        return assertType(expression, RightAssignmentExpression.class);
    }

    public static <T extends Statement> T assertType(Statement statement, Class<T> type) {
        assertEquals(type, statement.getClass(), "Unexpected statement type.");
        return type.cast(statement);
    }

    public static <T extends Expression> T assertType(Expression expression, Class<T> type) {
        assertEquals(type, expression.getClass(), "Unexpected expression type.");
        return type.cast(expression);
    }

    public static <T extends AtonementCrystal> T assertType(AtonementCrystal crystal, Class<T> type) {
        assertEquals(type, crystal.getClass(), "Unexpected crystal type.");
        return type.cast(crystal);
    }

    public static void assertLocation(Statement statement, CoordinatePair expectedLocation) {
        assertEquals(expectedLocation, statement.getLocation(), "Unexpected location.");
    }

    public static void assertLocation(Expression expression, CoordinatePair expectedLocation) {
        assertEquals(expectedLocation, expression.getLocation(), "Unexpected location.");
    }

    public static void assertLocation(AtonementCrystal crystal, CoordinatePair expectedLocation) {
        assertEquals(expectedLocation, crystal.getCoordinates(), "Unexpected location.");
    }

    public static void testLiteralExpression(Expression expression,
                                             Class<? extends ValueCrystal<?>> expectedLiteralType, Object expectedValue,
                                             CoordinatePair expectedLocation) {

        LiteralExpression literalExpression = assertType(expression, LiteralExpression.class);
        ValueCrystal<?> valueCrystal = assertType(literalExpression.getValue(), expectedLiteralType);
        assertEquals(expectedValue, valueCrystal.getValue(), "Unexpected value.");
        assertLocation(valueCrystal, expectedLocation);
    }

    public static void testOperator(AtonementCrystal operator, Class<? extends AtonementCrystal> expectedClass,
                             CoordinatePair expectedLocation) {
        assertType(operator, expectedClass);
        assertLocation(operator, expectedLocation);
    }
}
