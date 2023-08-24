package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_Numbers extends TreeWalkInterpreterTest_Base {
    private static final String INTEGER_MAX_VALUE = String.valueOf(Integer.MAX_VALUE);
    private static final String INTEGER_MIN_VALUE = String.valueOf(Integer.MIN_VALUE);
    private static final String LONG_MAX_VALUE = String.valueOf(Long.MAX_VALUE);
    private static final String LONG_MIN_VALUE = String.valueOf(Long.MIN_VALUE);

    private static final String FLOAT_MAX_VALUE = BigDecimal.valueOf(Float.MAX_VALUE).toPlainString() + ".0";
    private static final String FLOAT_MIN_VALUE = BigDecimal.valueOf(-Float.MAX_VALUE).toPlainString() + ".0";
    private static final String DOUBLE_MAX_VALUE = BigDecimal.valueOf(Double.MAX_VALUE).toPlainString() + ".0";
    private static final String DOUBLE_MIN_VALUE = BigDecimal.valueOf(-Double.MAX_VALUE).toPlainString() + ".0";

    private static final String INTEGER_MAX_VALUE_PLUS_ONE = String.valueOf(Integer.MAX_VALUE + 1L);
    private static final String INTEGER_MIN_VALUE_MINUS_ONE = String.valueOf(Integer.MIN_VALUE - 1L);
    private static final String LONG_MAX_VALUE_PLUS_ONE = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE).toString();
    private static final String LONG_MIN_VALUE_MINUS_ONE = BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.ONE).toString();

    // NOTE: 1e32 is used as adding or subtracting 1 is not enough to trigger an infinite value for a Float number.
    private static final String FLOAT_MAX_VALUE_PLUS_ONE = BigDecimal.valueOf(Float.MAX_VALUE).add(new BigDecimal("1e32")).toPlainString() + ".0";
    private static final String FLOAT_MIN_VALUE_MINUS_ONE = BigDecimal.valueOf(-Float.MAX_VALUE).subtract(new BigDecimal("1e32")).toPlainString() + ".0";

    // NOTE: 1e293 is used as adding or subtracting 1 is not enough to trigger an infinite value for a Double number.
    private static final String DOUBLE_MAX_VALUE_PLUS_ONE = BigDecimal.valueOf(Double.MAX_VALUE).add(new BigDecimal("1e293")).toPlainString() + ".0";
    private static final String DOUBLE_MIN_VALUE_MINUS_ONE = BigDecimal.valueOf(-Double.MAX_VALUE).subtract(new BigDecimal("1e293")).toPlainString() + ".0";

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_ExpressionStatement_IntegerLiteral() {
        testArithmeticExpression("42", 42, IntegerCrystal.class);
        testArithmeticExpression("22i", 22, IntegerCrystal.class);
        testArithmeticExpression("7I", 7, IntegerCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MAX_VALUE, Integer.MAX_VALUE, IntegerCrystal.class);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_ExpressionStatement_LongLiteral() {
        testArithmeticExpression("22l", 22L, LongCrystal.class);
        testArithmeticExpression("7L", 7L, LongCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MAX_VALUE + "L", (long) Integer.MAX_VALUE, LongCrystal.class);
        testArithmeticExpression(INTEGER_MAX_VALUE_PLUS_ONE, Integer.MAX_VALUE + 1L, LongCrystal.class);
        testArithmeticExpression(INTEGER_MAX_VALUE_PLUS_ONE + "L", Integer.MAX_VALUE + 1L, LongCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE, Long.MAX_VALUE, LongCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE + "L", Long.MAX_VALUE, LongCrystal.class);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_ExpressionStatement_BigIntegerLiteral() {
        testArithmeticExpression("512b", new BigInteger("512"), BigIntegerCrystal.class);
        testArithmeticExpression("1024B", new BigInteger("1024"), BigIntegerCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MAX_VALUE + "B", new BigInteger(INTEGER_MAX_VALUE), BigIntegerCrystal.class);
        testArithmeticExpression(INTEGER_MAX_VALUE_PLUS_ONE + "B", new BigInteger(INTEGER_MAX_VALUE_PLUS_ONE), BigIntegerCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE + "B", new BigInteger(LONG_MAX_VALUE), BigIntegerCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE_PLUS_ONE, new BigInteger(LONG_MAX_VALUE_PLUS_ONE), BigIntegerCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE_PLUS_ONE + "B", new BigInteger(LONG_MAX_VALUE_PLUS_ONE), BigIntegerCrystal.class);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_ExpressionStatement_FloatLiteral() {
        testArithmeticExpression("22f", 22F, FloatCrystal.class);
        testArithmeticExpression("7F", 7F, FloatCrystal.class);
        testArithmeticExpression("3.14f", 3.14F, FloatCrystal.class);
        testArithmeticExpression("6.28F", 6.28F, FloatCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MAX_VALUE + "F", Float.valueOf(INTEGER_MAX_VALUE), FloatCrystal.class);
        testArithmeticExpression(INTEGER_MAX_VALUE_PLUS_ONE + "F", Float.valueOf(INTEGER_MAX_VALUE_PLUS_ONE), FloatCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE + "F", Float.valueOf(LONG_MAX_VALUE), FloatCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE_PLUS_ONE + "F", Float.valueOf(LONG_MAX_VALUE_PLUS_ONE), FloatCrystal.class);
        testArithmeticExpression(FLOAT_MAX_VALUE + "F", Float.valueOf(FLOAT_MAX_VALUE), FloatCrystal.class);
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_ExpressionStatement_DoubleLiteral() {
        testArithmeticExpression("22d", 22D, DoubleCrystal.class);
        testArithmeticExpression("7D", 7D, DoubleCrystal.class);
        testArithmeticExpression("3.14d", 3.14D, DoubleCrystal.class);
        testArithmeticExpression("6.28D", 6.28D, DoubleCrystal.class);
        testArithmeticExpression("999.999", 999.999D, DoubleCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MAX_VALUE + "D", Double.valueOf(INTEGER_MAX_VALUE), DoubleCrystal.class);
        testArithmeticExpression(INTEGER_MAX_VALUE_PLUS_ONE + "D", Double.valueOf(INTEGER_MAX_VALUE_PLUS_ONE), DoubleCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE + "D", Double.valueOf(LONG_MAX_VALUE), DoubleCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE_PLUS_ONE + "D", Double.valueOf(LONG_MAX_VALUE_PLUS_ONE), DoubleCrystal.class);

        // NOTE: Float literals must be typed with the suffix f or F. Therefore, FLOAT_MAX_VALUE evaluates as a Double.
        testArithmeticExpression(FLOAT_MAX_VALUE, Double.valueOf(FLOAT_MAX_VALUE), DoubleCrystal.class);
        testArithmeticExpression(FLOAT_MAX_VALUE + "D", Double.valueOf(FLOAT_MAX_VALUE), DoubleCrystal.class);
        testArithmeticExpression(FLOAT_MAX_VALUE_PLUS_ONE, Double.valueOf(FLOAT_MAX_VALUE_PLUS_ONE), DoubleCrystal.class);
        testArithmeticExpression(FLOAT_MAX_VALUE_PLUS_ONE + "D", Double.valueOf(FLOAT_MAX_VALUE_PLUS_ONE), DoubleCrystal.class);
        testArithmeticExpression(DOUBLE_MAX_VALUE, Double.valueOf(DOUBLE_MAX_VALUE), DoubleCrystal.class);
        testArithmeticExpression(DOUBLE_MAX_VALUE + "D", Double.valueOf(DOUBLE_MAX_VALUE), DoubleCrystal.class);
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_ExpressionStatement_BigDecimalLiteral() {
        testArithmeticExpression("3.14b", new BigDecimal("3.14"), BigDecimalCrystal.class);
        testArithmeticExpression("6.28B", new BigDecimal("6.28"), BigDecimalCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MAX_VALUE + ".0B", new BigDecimal(INTEGER_MAX_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(INTEGER_MAX_VALUE_PLUS_ONE + ".0B", new BigDecimal(INTEGER_MAX_VALUE_PLUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE + ".0B", new BigDecimal(LONG_MAX_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(LONG_MAX_VALUE_PLUS_ONE + ".0B", new BigDecimal(LONG_MAX_VALUE_PLUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(FLOAT_MAX_VALUE + "B", new BigDecimal(FLOAT_MAX_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(FLOAT_MAX_VALUE_PLUS_ONE + "B", new BigDecimal(FLOAT_MAX_VALUE_PLUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(DOUBLE_MAX_VALUE + "B", new BigDecimal(DOUBLE_MAX_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(DOUBLE_MAX_VALUE_PLUS_ONE, new BigDecimal(DOUBLE_MAX_VALUE_PLUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(DOUBLE_MAX_VALUE_PLUS_ONE + "B", new BigDecimal(DOUBLE_MAX_VALUE_PLUS_ONE), BigDecimalCrystal.class);
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_ExpressionStatement_IntegerLiteral_Negated() {
        testArithmeticExpression("-42", -42, IntegerCrystal.class);
        testArithmeticExpression("22i", 22, IntegerCrystal.class);
        testArithmeticExpression("7I", 7, IntegerCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MIN_VALUE, Integer.MIN_VALUE, IntegerCrystal.class);
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_ExpressionStatement_LongLiteral_Negated() {
        testArithmeticExpression("-22L", -22L, LongCrystal.class);
        testArithmeticExpression("-7l", -7L, LongCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MIN_VALUE + "L", (long) Integer.MIN_VALUE, LongCrystal.class);
        testArithmeticExpression(INTEGER_MIN_VALUE_MINUS_ONE, Integer.MIN_VALUE - 1L, LongCrystal.class);
        testArithmeticExpression(INTEGER_MIN_VALUE_MINUS_ONE + "L", Integer.MIN_VALUE - 1L, LongCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE, Long.MIN_VALUE, LongCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE + "L", Long.MIN_VALUE, LongCrystal.class);
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_ExpressionStatement_BigIntegerLiteral_Negated() {
        testArithmeticExpression("-512B", new BigInteger("-512"), BigIntegerCrystal.class);
        testArithmeticExpression("-1024b", new BigInteger("-1024"), BigIntegerCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MIN_VALUE + "B", new BigInteger(INTEGER_MIN_VALUE), BigIntegerCrystal.class);
        testArithmeticExpression(INTEGER_MIN_VALUE_MINUS_ONE + "B", new BigInteger(INTEGER_MIN_VALUE_MINUS_ONE), BigIntegerCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE + "B", new BigInteger(LONG_MIN_VALUE), BigIntegerCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE_MINUS_ONE, new BigInteger(LONG_MIN_VALUE_MINUS_ONE), BigIntegerCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE_MINUS_ONE + "B", new BigInteger(LONG_MIN_VALUE_MINUS_ONE), BigIntegerCrystal.class);
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_ExpressionStatement_FloatLiteral_Negated() {
        testArithmeticExpression("-22F", -22F, FloatCrystal.class);
        testArithmeticExpression("-7f", -7F, FloatCrystal.class);
        testArithmeticExpression("-3.14F", -3.14F, FloatCrystal.class);
        testArithmeticExpression("-6.28f", -6.28F, FloatCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MIN_VALUE + "F", Float.valueOf(INTEGER_MIN_VALUE), FloatCrystal.class);
        testArithmeticExpression(INTEGER_MIN_VALUE_MINUS_ONE + "F", Float.valueOf(INTEGER_MIN_VALUE_MINUS_ONE), FloatCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE + "F", Float.valueOf(LONG_MIN_VALUE), FloatCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE_MINUS_ONE + "F", Float.valueOf(LONG_MIN_VALUE_MINUS_ONE), FloatCrystal.class);
        testArithmeticExpression(FLOAT_MIN_VALUE + "F", Float.valueOf(FLOAT_MIN_VALUE), FloatCrystal.class);
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_ExpressionStatement_DoubleLiteral_Negated() {
        testArithmeticExpression("-22D", -22D, DoubleCrystal.class);
        testArithmeticExpression("-7d", -7D, DoubleCrystal.class);
        testArithmeticExpression("-3.14D", -3.14D, DoubleCrystal.class);
        testArithmeticExpression("-6.28d", -6.28D, DoubleCrystal.class);
        testArithmeticExpression("-999.999", -999.999D, DoubleCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MIN_VALUE + "D", Double.valueOf(INTEGER_MIN_VALUE), DoubleCrystal.class);
        testArithmeticExpression(INTEGER_MIN_VALUE_MINUS_ONE + "D", Double.valueOf(INTEGER_MIN_VALUE_MINUS_ONE), DoubleCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE + "D", Double.valueOf(LONG_MIN_VALUE), DoubleCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE_MINUS_ONE + "D", Double.valueOf(LONG_MIN_VALUE_MINUS_ONE), DoubleCrystal.class);

        // NOTE: Float literals must be typed with the suffix f or F. Therefore, FLOAT_MIN_VALUE evaluates as a Double.
        testArithmeticExpression(FLOAT_MIN_VALUE, Double.valueOf(FLOAT_MIN_VALUE), DoubleCrystal.class);
        testArithmeticExpression(FLOAT_MIN_VALUE + "D", Double.valueOf(FLOAT_MIN_VALUE), DoubleCrystal.class);
        testArithmeticExpression(FLOAT_MIN_VALUE_MINUS_ONE, Double.valueOf(FLOAT_MIN_VALUE_MINUS_ONE), DoubleCrystal.class);
        testArithmeticExpression(FLOAT_MIN_VALUE_MINUS_ONE + "D", Double.valueOf(FLOAT_MIN_VALUE_MINUS_ONE), DoubleCrystal.class);
        testArithmeticExpression(DOUBLE_MIN_VALUE, Double.valueOf(DOUBLE_MIN_VALUE), DoubleCrystal.class);
        testArithmeticExpression(DOUBLE_MIN_VALUE + "D", Double.valueOf(DOUBLE_MIN_VALUE), DoubleCrystal.class);
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_ExpressionStatement_BigDecimalLiteral_Negated() {
        testArithmeticExpression("-3.14B", new BigDecimal("-3.14"), BigDecimalCrystal.class);
        testArithmeticExpression("-6.28b", new BigDecimal("-6.28"), BigDecimalCrystal.class);

        // Test numerical type threshold values.
        testArithmeticExpression(INTEGER_MIN_VALUE + ".0B", new BigDecimal(INTEGER_MIN_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(INTEGER_MIN_VALUE_MINUS_ONE + ".0B", new BigDecimal(INTEGER_MIN_VALUE_MINUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE + ".0B", new BigDecimal(LONG_MIN_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(LONG_MIN_VALUE_MINUS_ONE + ".0B", new BigDecimal(LONG_MIN_VALUE_MINUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(FLOAT_MIN_VALUE + "B", new BigDecimal(FLOAT_MIN_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(FLOAT_MIN_VALUE_MINUS_ONE + "B", new BigDecimal(FLOAT_MIN_VALUE_MINUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(DOUBLE_MIN_VALUE + "B", new BigDecimal(DOUBLE_MIN_VALUE), BigDecimalCrystal.class);
        testArithmeticExpression(DOUBLE_MIN_VALUE_MINUS_ONE, new BigDecimal(DOUBLE_MIN_VALUE_MINUS_ONE), BigDecimalCrystal.class);
        testArithmeticExpression(DOUBLE_MIN_VALUE_MINUS_ONE + "B", new BigDecimal(DOUBLE_MIN_VALUE_MINUS_ONE), BigDecimalCrystal.class);
    }
}
