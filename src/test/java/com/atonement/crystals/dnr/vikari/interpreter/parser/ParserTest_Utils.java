package com.atonement.crystals.dnr.vikari.interpreter.parser;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonement.crystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonement.crystals.dnr.vikari.core.expression.Expression;
import com.atonement.crystals.dnr.vikari.core.expression.LiteralExpression;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Helper methods for Parser tests.
 */
public class ParserTest_Utils {
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
    }

    public static void testLongLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, LongCrystal.class);
    }

    public static void testBigIntegerLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, BigIntegerCrystal.class);
    }

    public static void testFloatLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, FloatCrystal.class);
    }

    public static void testDoubleLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, DoubleCrystal.class);
    }

    public static void testBigDecimalLiteralExpression(Expression expr, Object expectedValue) {
        testNumberCrystalLiteralExpression(expr, expectedValue, BigDecimalCrystal.class);
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
