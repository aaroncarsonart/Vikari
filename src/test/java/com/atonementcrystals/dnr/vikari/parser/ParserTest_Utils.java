package com.atonementcrystals.dnr.vikari.parser;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
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

import static org.junit.jupiter.api.Assertions.*;

/**
 * Helper methods for Parser tests.
 */
public class ParserTest_Utils {
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
}
