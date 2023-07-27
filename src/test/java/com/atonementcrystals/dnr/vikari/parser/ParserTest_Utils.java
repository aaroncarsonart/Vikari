package com.atonementcrystals.dnr.vikari.parser;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeHierarchy;
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
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.NullLiteralExpression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static com.atonementcrystals.dnr.vikari.TestUtils.testNumberCrystal;
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

    private static void testTypeCrystalsOfLiteral(AtonementCrystal literalCrystal, VikariType vikariType) {
        TypeCrystal expectedType = vikariType.getTypeCrystal();

        TypeCrystal declaredType = literalCrystal.getDeclaredType();
        assertEquals(declaredType, expectedType, "Unexpected declared type.");

        TypeCrystal instantiatedType = literalCrystal.getInstantiatedType();
        assertEquals(instantiatedType, expectedType, "Unexpected instantiated type.");
    }

    private static void testNumberCrystalLiteralExpression(Expression expr, Object expectedValue,
                                                           Class<? extends NumberCrystal> clazz) {
        assertEquals(LiteralExpression.class, expr.getClass(), "Unexpected expression type.");

        LiteralExpression literalExpression = (LiteralExpression) expr;
        AtonementCrystal value = literalExpression.getValue();
        assertEquals(clazz, value.getClass(), "Unexpected literal type.");

        NumberCrystal number = clazz.cast(value);
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
            TestUtils.testNumberCrystal(rvalue, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
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
}
