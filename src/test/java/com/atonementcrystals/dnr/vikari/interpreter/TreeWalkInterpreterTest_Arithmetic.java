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
import java.math.RoundingMode;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_Arithmetic extends TreeWalkInterpreterTest_Base {

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_IntegerResult() {
        testArithmeticExpression("7 + 22", 7 + 22, IntegerCrystal.class);
        testArithmeticExpression("7 - 22", 7 - 22, IntegerCrystal.class);
        testArithmeticExpression("7 * 22", 7 * 22, IntegerCrystal.class);
        testArithmeticExpression("7 / 22", 7 / 22, IntegerCrystal.class);
        testArithmeticExpression("7 \\ 22", 22 / 7, IntegerCrystal.class);
        // with grouping
        testArithmeticExpression("[7 + 22]", (7 + 22), IntegerCrystal.class);
        testArithmeticExpression("[7 - 22]", (7 - 22), IntegerCrystal.class);
        testArithmeticExpression("[7 * 22]", (7 * 22), IntegerCrystal.class);
        testArithmeticExpression("[7 / 22]", (7 / 22), IntegerCrystal.class);
        testArithmeticExpression("[7 \\ 22]", (22 / 7), IntegerCrystal.class);
        // negated
        testArithmeticExpression("-[7 + 22]", -(7 + 22), IntegerCrystal.class);
        testArithmeticExpression("-[7 - 22]", -(7 - 22), IntegerCrystal.class);
        testArithmeticExpression("-[7 * 22]", -(7 * 22), IntegerCrystal.class);
        testArithmeticExpression("-[7 / 22]", -(7 / 22), IntegerCrystal.class);
        testArithmeticExpression("-[7 \\ 22]", -(22 / 7), IntegerCrystal.class);
        // complex expression
        testArithmeticExpression("7 + [22 - 3] / [17 * 32]", 7 + (22 - 3) / (17 * 32), IntegerCrystal.class);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_LongResult() {
        testArithmeticExpression("7L + 22L", 7L + 22L, LongCrystal.class);
        testArithmeticExpression("7L - 22L", 7L - 22L, LongCrystal.class);
        testArithmeticExpression("7L * 22L", 7L * 22L, LongCrystal.class);
        testArithmeticExpression("7L / 22L", 7L / 22L, LongCrystal.class);
        testArithmeticExpression("7L \\ 22L", 22L / 7L, LongCrystal.class);
        // with grouping
        testArithmeticExpression("[7L + 22L]", (7L + 22L), LongCrystal.class);
        testArithmeticExpression("[7L - 22L]", (7L - 22L), LongCrystal.class);
        testArithmeticExpression("[7L * 22L]", (7L * 22L), LongCrystal.class);
        testArithmeticExpression("[7L / 22L]", (7L / 22L), LongCrystal.class);
        testArithmeticExpression("[7L \\ 22L]", (22L / 7L), LongCrystal.class);
        // negated
        testArithmeticExpression("-[7L + 22L]", -(7L + 22L), LongCrystal.class);
        testArithmeticExpression("-[7L - 22L]", -(7L - 22L), LongCrystal.class);
        testArithmeticExpression("-[7L * 22L]", -(7L * 22L), LongCrystal.class);
        testArithmeticExpression("-[7L / 22L]", -(7L / 22L), LongCrystal.class);
        testArithmeticExpression("-[7L \\ 22L]", -(22L / 7L), LongCrystal.class);
        // complex expression
        testArithmeticExpression("7L + [22L - 3L] / [17L * 32L]", 7L + (22L - 3L) / (17L * 32L), LongCrystal.class);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_BigIntegerResult() {
        testArithmeticExpression("7B + 22B", BigInteger.valueOf(7L + 22L), BigIntegerCrystal.class);
        testArithmeticExpression("7B - 22B", BigInteger.valueOf(7L - 22L), BigIntegerCrystal.class);
        testArithmeticExpression("7B * 22B", BigInteger.valueOf(7L * 22L), BigIntegerCrystal.class);
        testArithmeticExpression("7B / 22B", BigInteger.valueOf(7L / 22L), BigIntegerCrystal.class);
        testArithmeticExpression("7B \\ 22B", BigInteger.valueOf(22L / 7L), BigIntegerCrystal.class);
        // with grouping
        testArithmeticExpression("[7B + 22B]", BigInteger.valueOf(7L + 22L), BigIntegerCrystal.class);
        testArithmeticExpression("[7B - 22B]", BigInteger.valueOf(7L - 22L), BigIntegerCrystal.class);
        testArithmeticExpression("[7B * 22B]", BigInteger.valueOf(7L * 22L), BigIntegerCrystal.class);
        testArithmeticExpression("[7B / 22B]", BigInteger.valueOf(7L / 22L), BigIntegerCrystal.class);
        testArithmeticExpression("[7B \\ 22B]", BigInteger.valueOf(22L / 7L), BigIntegerCrystal.class);
        // negated
        testArithmeticExpression("-[7B + 22B]", BigInteger.valueOf(7L + 22L).negate(), BigIntegerCrystal.class);
        testArithmeticExpression("-[7B - 22B]", BigInteger.valueOf(7L - 22L).negate(), BigIntegerCrystal.class);
        testArithmeticExpression("-[7B * 22B]", BigInteger.valueOf(7L * 22L).negate(), BigIntegerCrystal.class);
        testArithmeticExpression("-[7B / 22B]", BigInteger.valueOf(7L / 22L).negate(), BigIntegerCrystal.class);
        testArithmeticExpression("-[7B \\ 22B]", BigInteger.valueOf(22L / 7L).negate(), BigIntegerCrystal.class);
        // complex expression
        testArithmeticExpression("7B + [22B - 3B] / [17B * 32B]", BigInteger.valueOf(7L + (22L - 3L) / (17L * 32L)), BigIntegerCrystal.class);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_FloatResult_NoFractionalPart() {
        testArithmeticExpression("7F + 22F", 7F + 22F, FloatCrystal.class);
        testArithmeticExpression("7F - 22F", 7F - 22F, FloatCrystal.class);
        testArithmeticExpression("7F * 22F", 7F * 22F, FloatCrystal.class);
        testArithmeticExpression("7F / 22F", 7F / 22F, FloatCrystal.class);
        testArithmeticExpression("7F \\ 22F", 22F / 7F, FloatCrystal.class);
        // with grouping
        testArithmeticExpression("[7F + 22F]", (7F + 22F), FloatCrystal.class);
        testArithmeticExpression("[7F - 22F]", (7F - 22F), FloatCrystal.class);
        testArithmeticExpression("[7F * 22F]", (7F * 22F), FloatCrystal.class);
        testArithmeticExpression("[7F / 22F]", (7F / 22F), FloatCrystal.class);
        testArithmeticExpression("[7F \\ 22F]", (22F / 7F), FloatCrystal.class);
        // negated
        testArithmeticExpression("-[7F + 22F]", -(7F + 22F), FloatCrystal.class);
        testArithmeticExpression("-[7F - 22F]", -(7F - 22F), FloatCrystal.class);
        testArithmeticExpression("-[7F * 22F]", -(7F * 22F), FloatCrystal.class);
        testArithmeticExpression("-[7F / 22F]", -(7F / 22F), FloatCrystal.class);
        testArithmeticExpression("-[7F \\ 22F]", -(22F / 7F), FloatCrystal.class);
        // complex expression
        testArithmeticExpression("7F + [22F - 3F] / [17F * 32F]", 7F + (22F - 3F) / (17F * 32F), FloatCrystal.class);
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_DoubleResult_NoFractionalPart() {
        testArithmeticExpression("7F + 22D", 7D + 22D, DoubleCrystal.class);
        testArithmeticExpression("7D - 22D", 7D - 22D, DoubleCrystal.class);
        testArithmeticExpression("7D * 22D", 7D * 22D, DoubleCrystal.class);
        testArithmeticExpression("7D / 22D", 7D / 22D, DoubleCrystal.class);
        testArithmeticExpression("7D \\ 22D", 22D / 7D, DoubleCrystal.class);
        // with grouping
        testArithmeticExpression("[7D + 22D]", (7D + 22D), DoubleCrystal.class);
        testArithmeticExpression("[7D - 22D]", (7D - 22D), DoubleCrystal.class);
        testArithmeticExpression("[7D * 22D]", (7D * 22D), DoubleCrystal.class);
        testArithmeticExpression("[7D / 22D]", (7D / 22D), DoubleCrystal.class);
        testArithmeticExpression("[7D \\ 22D]", (22D / 7D), DoubleCrystal.class);
        // negated
        testArithmeticExpression("-[7D + 22D]", -(7D + 22D), DoubleCrystal.class);
        testArithmeticExpression("-[7D - 22D]", -(7D - 22D), DoubleCrystal.class);
        testArithmeticExpression("-[7D * 22D]", -(7D * 22D), DoubleCrystal.class);
        testArithmeticExpression("-[7D / 22D]", -(7D / 22D), DoubleCrystal.class);
        testArithmeticExpression("-[7D \\ 22D]", -(22D / 7D), DoubleCrystal.class);
        // complex expression
        testArithmeticExpression("7D + [22D - 3D] / [17D * 32D]", 7D + (22D - 3D) / (17D * 32D), DoubleCrystal.class);
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_FloatResult_WithFractionalPart() {
        testArithmeticExpression("22.7F + 3.14F", 22.7F + 3.14F, FloatCrystal.class);
        testArithmeticExpression("22.7F - 3.14F", 22.7F - 3.14F, FloatCrystal.class);
        testArithmeticExpression("22.7F * 3.14F", 22.7F * 3.14F, FloatCrystal.class);
        testArithmeticExpression("22.7F / 3.14F", 22.7F / 3.14F, FloatCrystal.class);
        testArithmeticExpression("22.7F \\ 3.14F", 3.14F / 22.7F, FloatCrystal.class);
        // with grouping
        testArithmeticExpression("[22.7F + 3.14F]", (22.7F + 3.14F), FloatCrystal.class);
        testArithmeticExpression("[22.7F - 3.14F]", (22.7F - 3.14F), FloatCrystal.class);
        testArithmeticExpression("[22.7F * 3.14F]", (22.7F * 3.14F), FloatCrystal.class);
        testArithmeticExpression("[22.7F / 3.14F]", (22.7F / 3.14F), FloatCrystal.class);
        testArithmeticExpression("[22.7F \\ 3.14F]", (3.14F / 22.7F), FloatCrystal.class);
        // negated
        testArithmeticExpression("-[22.7F + 3.14F]", -(22.7F + 3.14F), FloatCrystal.class);
        testArithmeticExpression("-[22.7F - 3.14F]", -(22.7F - 3.14F), FloatCrystal.class);
        testArithmeticExpression("-[22.7F * 3.14F]", -(22.7F * 3.14F), FloatCrystal.class);
        testArithmeticExpression("-[22.7F / 3.14F]", -(22.7F / 3.14F), FloatCrystal.class);
        testArithmeticExpression("-[22.7F \\ 3.14F]", -(3.14F / 22.7F), FloatCrystal.class);
        // complex expression
        testArithmeticExpression("22.7F + [17.4F - 0.3F] / [0.17F * 32.9F]", 22.7F + (17.4F - 0.3F) / (0.17F * 32.9F), FloatCrystal.class);
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_DoubleResult_WithFractionalPart() {
        testArithmeticExpression("22.7D + 3.14D", 22.7D + 3.14D, DoubleCrystal.class);
        testArithmeticExpression("22.7D - 3.14D", 22.7D - 3.14D, DoubleCrystal.class);
        testArithmeticExpression("22.7D * 3.14D", 22.7D * 3.14D, DoubleCrystal.class);
        testArithmeticExpression("22.7D / 3.14D", 22.7D / 3.14D, DoubleCrystal.class);
        testArithmeticExpression("22.7D \\ 3.14D", 3.14D / 22.7D, DoubleCrystal.class);
        // with grouping
        testArithmeticExpression("[22.7D + 3.14D]", (22.7D + 3.14D), DoubleCrystal.class);
        testArithmeticExpression("[22.7D - 3.14D]", (22.7D - 3.14D), DoubleCrystal.class);
        testArithmeticExpression("[22.7D * 3.14D]", (22.7D * 3.14D), DoubleCrystal.class);
        testArithmeticExpression("[22.7D / 3.14D]", (22.7D / 3.14D), DoubleCrystal.class);
        testArithmeticExpression("[22.7D \\ 3.14D]", (3.14D / 22.7D), DoubleCrystal.class);
        // negated
        testArithmeticExpression("-[22.7D + 3.14D]", -(22.7D + 3.14D), DoubleCrystal.class);
        testArithmeticExpression("-[22.7D - 3.14D]", -(22.7D - 3.14D), DoubleCrystal.class);
        testArithmeticExpression("-[22.7D * 3.14D]", -(22.7D * 3.14D), DoubleCrystal.class);
        testArithmeticExpression("-[22.7D / 3.14D]", -(22.7D / 3.14D), DoubleCrystal.class);
        testArithmeticExpression("-[22.7D \\ 3.14D]", -(3.14D / 22.7D), DoubleCrystal.class);
        // complex expression
        testArithmeticExpression("22.7D + [17.4D - 0.3D] / [0.17D * 32.9D]", 22.7D + (17.4D - 0.3D) / (0.17D * 32.9D), DoubleCrystal.class);
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_BigDecimalResult_WithFractionalPart() {
        Class<BigDecimalCrystal> expectedClass = BigDecimalCrystal.class;
        int scale = Arithmetic.getScale();
        RoundingMode roundingMode = Arithmetic.getRoundingMode();

        BigDecimal firstOperand = new BigDecimal("22.7");
        BigDecimal secondOperand = new BigDecimal("3.14");

        // add
        String sourceString = "22.7B + 3.14B";
        BigDecimal expectedValue = firstOperand.add(secondOperand);
        testArithmeticExpression(sourceString, expectedValue, expectedClass);
        testArithmeticExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testArithmeticExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // subtract
        sourceString = "22.7B - 3.14B";
        expectedValue = firstOperand.subtract(secondOperand);
        testArithmeticExpression(sourceString, expectedValue, expectedClass);
        testArithmeticExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testArithmeticExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // multiply
        sourceString = "22.7B * 3.14B";
        expectedValue = firstOperand.multiply(secondOperand);
        testArithmeticExpression(sourceString, expectedValue, expectedClass);
        testArithmeticExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testArithmeticExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // left divide
        sourceString = "22.7B / 3.14B";
        expectedValue = firstOperand.divide(secondOperand, scale, roundingMode);
        testArithmeticExpression(sourceString, expectedValue, expectedClass);
        testArithmeticExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testArithmeticExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // right divide
        sourceString = "22.7B \\ 3.14B";
        expectedValue = secondOperand.divide(firstOperand, scale, roundingMode);
        testArithmeticExpression(sourceString, expectedValue, expectedClass);
        testArithmeticExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testArithmeticExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // complex expression
        BigDecimal thirdOperand, fourthOperand, fifthOperand;

        secondOperand = new BigDecimal("17.4");
        thirdOperand = new BigDecimal("0.3");
        fourthOperand = new BigDecimal("0.17");
        fifthOperand = new BigDecimal("32.9");

        sourceString = "22.7B + [17.4B - 0.3B] / [0.17B * 32.9B]";

        BigDecimal divisor = secondOperand.subtract(thirdOperand);
        BigDecimal dividend = fourthOperand.multiply(fifthOperand);
        expectedValue = firstOperand.add(divisor.divide(dividend, scale, roundingMode));

        testArithmeticExpression(sourceString, expectedValue, expectedClass);
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_IntToLong() {
        testArithmeticExpression("22 + 7L", 29L, LongCrystal.class);
        testArithmeticExpression("22L + 7", 29L, LongCrystal.class);
        testArithmeticExpression("22L - 7", 15L, LongCrystal.class);
        testArithmeticExpression("22 - 7L", 15L, LongCrystal.class);
        testArithmeticExpression("22L * 7", 154L, LongCrystal.class);
        testArithmeticExpression("22 * 7L", 154L, LongCrystal.class);
        testArithmeticExpression("22L / 7", 3L, LongCrystal.class);
        testArithmeticExpression("22 / 7L", 3L, LongCrystal.class);
        testArithmeticExpression("22L \\ 7", 0L, LongCrystal.class);
        testArithmeticExpression("22 \\ 7L", 0L, LongCrystal.class);
        testArithmeticExpression("2147483647 + 1", 2147483648L, LongCrystal.class);
        testArithmeticExpression("-2147483648 - 1", -2147483649L, LongCrystal.class);
        testArithmeticExpression("2147483647 * 2", 4294967294L, LongCrystal.class);
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_LongToBigInt() {
        testArithmeticExpression("22L + 7B", new BigInteger("29"), BigIntegerCrystal.class);
        testArithmeticExpression("22B + 7L", new BigInteger("29"), BigIntegerCrystal.class);
        testArithmeticExpression("22L - 7B", new BigInteger("15"), BigIntegerCrystal.class);
        testArithmeticExpression("22B - 7L", new BigInteger("15"), BigIntegerCrystal.class);
        testArithmeticExpression("22L * 7B", new BigInteger("154"), BigIntegerCrystal.class);
        testArithmeticExpression("22B * 7L", new BigInteger("154"), BigIntegerCrystal.class);
        testArithmeticExpression("22L / 7B", new BigInteger("3"), BigIntegerCrystal.class);
        testArithmeticExpression("22B / 7L", new BigInteger("3"), BigIntegerCrystal.class);
        testArithmeticExpression("22L \\ 7B", new BigInteger("0"), BigIntegerCrystal.class);
        testArithmeticExpression("22B \\ 7L", new BigInteger("0"), BigIntegerCrystal.class);

        testArithmeticExpression("9223372036854775807 + 1L", new BigInteger("9223372036854775808"), BigIntegerCrystal.class);
        testArithmeticExpression("-9223372036854775808 - 1L", new BigInteger("-9223372036854775809"), BigIntegerCrystal.class);
        testArithmeticExpression("9223372036854775807 * 2L", new BigInteger("18446744073709551614"), BigIntegerCrystal.class);
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_IntToBigInt() {
        testArithmeticExpression("22 + 7B", new BigInteger("29"), BigIntegerCrystal.class);
        testArithmeticExpression("22B + 7", new BigInteger("29"), BigIntegerCrystal.class);
        testArithmeticExpression("22 - 7B", new BigInteger("15"), BigIntegerCrystal.class);
        testArithmeticExpression("22B - 7", new BigInteger("15"), BigIntegerCrystal.class);
        testArithmeticExpression("22 * 7B", new BigInteger("154"), BigIntegerCrystal.class);
        testArithmeticExpression("22B * 7", new BigInteger("154"), BigIntegerCrystal.class);
        testArithmeticExpression("22 / 7B", new BigInteger("3"), BigIntegerCrystal.class);
        testArithmeticExpression("22B / 7", new BigInteger("3"), BigIntegerCrystal.class);
        testArithmeticExpression("22 \\ 7B", new BigInteger("0"), BigIntegerCrystal.class);
        testArithmeticExpression("22B \\ 7", new BigInteger("0"), BigIntegerCrystal.class);

        testArithmeticExpression("9223372036854775807 + 1", new BigInteger("9223372036854775808"), BigIntegerCrystal.class);
        testArithmeticExpression("-9223372036854775808 - 1", new BigInteger("-9223372036854775809"), BigIntegerCrystal.class);
        testArithmeticExpression("9223372036854775807 * 2", new BigInteger("18446744073709551614"), BigIntegerCrystal.class);

        testArithmeticExpression("2147483647 * 2147483647 * 3", new BigInteger("13835058042397261827"), BigIntegerCrystal.class);
        testArithmeticExpression("-[2147483647 * 2147483647 * 3]", new BigInteger("-13835058042397261827"), BigIntegerCrystal.class);
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_FloatToDouble() {
        testArithmeticExpression("3.14F + 6.28D", 9.42D, DoubleCrystal.class);
        testArithmeticExpression("3.14D + 6.28F", 9.42D, DoubleCrystal.class);
        testArithmeticExpression("3.14F - 6.28D", -3.14D, DoubleCrystal.class);
        testArithmeticExpression("3.14D - 6.28F", -3.14D, DoubleCrystal.class);
        testArithmeticExpression("3.14F * 6.28D", 19.7192D, DoubleCrystal.class);
        testArithmeticExpression("3.14D * 6.28F", 19.7192D, DoubleCrystal.class);
        testArithmeticExpression("3.14F / 6.28D", 0.5D, DoubleCrystal.class);
        testArithmeticExpression("3.14D / 6.28F", 0.5D, DoubleCrystal.class);
        testArithmeticExpression("3.14F \\ 6.28D", 2.0D, DoubleCrystal.class);
        testArithmeticExpression("3.14D \\ 6.28F", 2.0D, DoubleCrystal.class);
    }

    @Test
    @Order(13)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_FloatToBigDecimal() {
        testArithmeticExpression("3.14F + 6.28B", new BigDecimal("9.42"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B + 6.28F", new BigDecimal("9.42"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14F - 6.28B", new BigDecimal("-3.14"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B - 6.28F", new BigDecimal("-3.14"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14F * 6.28B", new BigDecimal("19.7192"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B * 6.28F", new BigDecimal("19.7192"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14F / 6.28B", new BigDecimal("0.5"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B / 6.28F", new BigDecimal("0.5"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14F \\ 6.28B", new BigDecimal("2"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B \\ 6.28F", new BigDecimal("2"), BigDecimalCrystal.class);
    }

    @Test
    @Order(14)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_DoubleToBigDecimal() {
        testArithmeticExpression("3.14D + 6.28B", new BigDecimal("9.42"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B + 6.28D", new BigDecimal("9.42"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14D - 6.28B", new BigDecimal("-3.14"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B - 6.28D", new BigDecimal("-3.14"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14D * 6.28B", new BigDecimal("19.7192"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B * 6.28D", new BigDecimal("19.7192"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14D / 6.28B", new BigDecimal("0.5"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B / 6.28D", new BigDecimal("0.5"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14D \\ 6.28B", new BigDecimal("2"), BigDecimalCrystal.class);
        testArithmeticExpression("3.14B \\ 6.28D", new BigDecimal("2"), BigDecimalCrystal.class);
    }

    @Test
    @Order(15)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_IntToFloat() {
        testArithmeticExpression("22F + 7", 29F, FloatCrystal.class);
        testArithmeticExpression("22 + 7F", 29F, FloatCrystal.class);
        testArithmeticExpression("22F - 7", 15F, FloatCrystal.class);
        testArithmeticExpression("22 - 7F", 15F, FloatCrystal.class);
        testArithmeticExpression("22F * 7", 154F, FloatCrystal.class);
        testArithmeticExpression("22 * 7F", 154F, FloatCrystal.class);
        testArithmeticExpression("22F / 7", 3.142857F, FloatCrystal.class);
        testArithmeticExpression("22 / 7F", 3.142857F, FloatCrystal.class);
        testArithmeticExpression("22F \\ 7", 0.3181818F, FloatCrystal.class);
        testArithmeticExpression("22 \\ 7F", 0.3181818F, FloatCrystal.class);
    }

    @Test
    @Order(16)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_LongToFloat() {
        testArithmeticExpression("22F + 7L", 29F, FloatCrystal.class);
        testArithmeticExpression("22L + 7F", 29F, FloatCrystal.class);
        testArithmeticExpression("22F - 7L", 15F, FloatCrystal.class);
        testArithmeticExpression("22L - 7F", 15F, FloatCrystal.class);
        testArithmeticExpression("22F * 7", 154F, FloatCrystal.class);
        testArithmeticExpression("22L * 7F", 154F, FloatCrystal.class);
        testArithmeticExpression("22F / 7L", 3.142857F, FloatCrystal.class);
        testArithmeticExpression("22L / 7F", 3.142857F, FloatCrystal.class);
        testArithmeticExpression("22F \\ 7L", 0.3181818F, FloatCrystal.class);
        testArithmeticExpression("22L \\ 7F", 0.3181818F, FloatCrystal.class);
    }

    @Test
    @Order(17)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_BigIntToFloat() {
        testArithmeticExpression("22F + 7B", 29F, FloatCrystal.class);
        testArithmeticExpression("22B + 7F", 29F, FloatCrystal.class);
        testArithmeticExpression("22F - 7B", 15F, FloatCrystal.class);
        testArithmeticExpression("22B - 7F", 15F, FloatCrystal.class);
        testArithmeticExpression("22F * 7B", 154F, FloatCrystal.class);
        testArithmeticExpression("22B * 7F", 154F, FloatCrystal.class);
        testArithmeticExpression("22F / 7B", 3.142857F, FloatCrystal.class);
        testArithmeticExpression("22B / 7F", 3.142857F, FloatCrystal.class);
        testArithmeticExpression("22F \\ 7B", 0.3181818F, FloatCrystal.class);
        testArithmeticExpression("22B \\ 7F", 0.3181818F, FloatCrystal.class);
    }

    @Test
    @Order(18)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_IntToDouble() {
        testArithmeticExpression("22D + 7", 29D, DoubleCrystal.class);
        testArithmeticExpression("22 + 7D", 29D, DoubleCrystal.class);
        testArithmeticExpression("22D - 7", 15D, DoubleCrystal.class);
        testArithmeticExpression("22 - 7D", 15D, DoubleCrystal.class);
        testArithmeticExpression("22D * 7", 154D, DoubleCrystal.class);
        testArithmeticExpression("22 * 7D", 154D, DoubleCrystal.class);
        testArithmeticExpression("22D / 7", 3.142857142857143D, DoubleCrystal.class);
        testArithmeticExpression("22 / 7D", 3.142857142857143D, DoubleCrystal.class);
        testArithmeticExpression("22D \\ 7", 0.3181818181818182D, DoubleCrystal.class);
        testArithmeticExpression("22 \\ 7D", 0.3181818181818182D, DoubleCrystal.class);
    }

    @Test
    @Order(19)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_LongToDouble() {
        testArithmeticExpression("22D + 7L", 29D, DoubleCrystal.class);
        testArithmeticExpression("22L + 7D", 29D, DoubleCrystal.class);
        testArithmeticExpression("22D - 7L", 15D, DoubleCrystal.class);
        testArithmeticExpression("22L - 7D", 15D, DoubleCrystal.class);
        testArithmeticExpression("22D * 7", 154D, DoubleCrystal.class);
        testArithmeticExpression("22L * 7D", 154D, DoubleCrystal.class);
        testArithmeticExpression("22D / 7L", 3.142857142857143D, DoubleCrystal.class);
        testArithmeticExpression("22L / 7D", 3.142857142857143D, DoubleCrystal.class);
        testArithmeticExpression("22D \\ 7L", 0.3181818181818182D, DoubleCrystal.class);
        testArithmeticExpression("22L \\ 7D", 0.3181818181818182D, DoubleCrystal.class);
    }

    @Test
    @Order(20)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_BigIntToDouble() {
        testArithmeticExpression("22D + 7B", 29D, DoubleCrystal.class);
        testArithmeticExpression("22B + 7D", 29D, DoubleCrystal.class);
        testArithmeticExpression("22D - 7B", 15D, DoubleCrystal.class);
        testArithmeticExpression("22B - 7D", 15D, DoubleCrystal.class);
        testArithmeticExpression("22D * 7B", 154D, DoubleCrystal.class);
        testArithmeticExpression("22B * 7D", 154D, DoubleCrystal.class);
        testArithmeticExpression("22D / 7B", 3.142857142857143D, DoubleCrystal.class);
        testArithmeticExpression("22B / 7D", 3.142857142857143D, DoubleCrystal.class);
        testArithmeticExpression("22D \\ 7B", 0.3181818181818182D, DoubleCrystal.class);
        testArithmeticExpression("22B \\ 7D", 0.3181818181818182D, DoubleCrystal.class);
    }

    @Test
    @Order(21)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_IntToBigDecimal() {
        testArithmeticExpression("22.0B + 7", new BigDecimal("29.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22 + 7.0B", new BigDecimal("29.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22.0B - 7", new BigDecimal("15.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22 - 7.0B", new BigDecimal("15.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22.0B * 7", new BigDecimal("154.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22 * 7.0B", new BigDecimal("154.0"), BigDecimalCrystal.class);

        BigDecimal expectedResult = new BigDecimal("3.14285714285714285714285714285714");
        testArithmeticExpression("22.0B / 7", expectedResult, BigDecimalCrystal.class);
        testArithmeticExpression("22 / 7.0B", expectedResult, BigDecimalCrystal.class);

        expectedResult = new BigDecimal("0.31818181818181818181818181818182");
        testArithmeticExpression("22.0B \\ 7", expectedResult, BigDecimalCrystal.class);
        testArithmeticExpression("22 \\ 7.0B", expectedResult, BigDecimalCrystal.class);
    }

    @Test
    @Order(22)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_LongToBigDecimal() {
        testArithmeticExpression("22.0B + 7L", new BigDecimal("29.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22L + 7.0B", new BigDecimal("29.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22.0B - 7L", new BigDecimal("15.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22L - 7.0B", new BigDecimal("15.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22.0B * 7", new BigDecimal("154.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22L * 7.0B", new BigDecimal("154.0"), BigDecimalCrystal.class);

        BigDecimal expectedResult = new BigDecimal("3.14285714285714285714285714285714");
        testArithmeticExpression("22.0B / 7L", expectedResult, BigDecimalCrystal.class);
        testArithmeticExpression("22L / 7.0B", expectedResult, BigDecimalCrystal.class);

        expectedResult = new BigDecimal("0.31818181818181818181818181818182");
        testArithmeticExpression("22.0B \\ 7L", expectedResult, BigDecimalCrystal.class);
        testArithmeticExpression("22L \\ 7.0B", expectedResult, BigDecimalCrystal.class);
    }

    @Test
    @Order(23)
    public void testTreeWalkInterpreter_ExpressionStatement_PromotionOfNumericTypes_BigIntToBigDecimal() {
        testArithmeticExpression("22.0B + 7B", new BigDecimal("29.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22B + 7.0B", new BigDecimal("29.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22.0B - 7B", new BigDecimal("15.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22B - 7.0B", new BigDecimal("15.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22.0B * 7B", new BigDecimal("154.0"), BigDecimalCrystal.class);
        testArithmeticExpression("22B * 7.0B", new BigDecimal("154.0"), BigDecimalCrystal.class);

        BigDecimal expectedResult = new BigDecimal("3.14285714285714285714285714285714");
        testArithmeticExpression("22.0B / 7B", expectedResult, BigDecimalCrystal.class);
        testArithmeticExpression("22B / 7.0B", expectedResult, BigDecimalCrystal.class);

        expectedResult = new BigDecimal("0.31818181818181818181818181818182");
        testArithmeticExpression("22.0B \\ 7B", expectedResult, BigDecimalCrystal.class);
        testArithmeticExpression("22B \\ 7.0B", expectedResult, BigDecimalCrystal.class);
    }

    /**
     * BigDecimals use the largest scale necessary for all arithmetic operations besides division, for which the default
     * scale is 32. But literals with a scale larger than 32 will apply that scale to the division operator instead. So
     * this test ensures that all arithmetic operations involving BigDecimals have the appropriate scale applied.
     */
    @Test
    @Order(24)
    public void testTreeWalkInterpreter_ExpressionStatement_WithBigDecimals_TestScale() {
        // addition
        testBigDecimalScale("3.0B + 2.0B", new BigDecimal("5.0"));
        testBigDecimalScale("3.000B + 2.0B", new BigDecimal("5.000"));
        testBigDecimalScale("3.3333333333B + 2.2222222222B", new BigDecimal("5.5555555555"));
        testBigDecimalScale("3." + "3".repeat(32) + "B + 2.0B", new BigDecimal("5." + "3".repeat(32)));
        testBigDecimalScale("3." + "3".repeat(64) + "B + 2.0B", new BigDecimal("5." + "3".repeat(64)));
        testBigDecimalScale("2.0B + 3." + "3".repeat(64) + "B", new BigDecimal("5." + "3".repeat(64))); // swap operands

        // subtraction
        testBigDecimalScale("3.0B - 2.0B", new BigDecimal("1.0"));
        testBigDecimalScale("3.000B - 2.0B", new BigDecimal("1.000"));
        testBigDecimalScale("3.3333333333B - 2.2222222222B", new BigDecimal("1.1111111111"));
        testBigDecimalScale("3." + "3".repeat(32) + "B - 2.0B", new BigDecimal("1." + "3".repeat(32)));
        testBigDecimalScale("3." + "3".repeat(64) + "B - 2.0B", new BigDecimal("1." + "3".repeat(64)));
        testBigDecimalScale("2.0B - 1." + "1".repeat(64) + "B", new BigDecimal("0." + "8".repeat(63) + "9")); // swap operands

        // multiplication
        testBigDecimalScale("3.0B * 2.0B", new BigDecimal("6.00"));
        testBigDecimalScale("3.000B * 2.0B", new BigDecimal("6.0000"));
        testBigDecimalScale("3.3333333333B * 2.2222222222B", new BigDecimal("7.40740740725925925926"));
        testBigDecimalScale("3." + "3".repeat(32) + "B * 2.0B", new BigDecimal("6." + "6".repeat(32) + "0"));
        testBigDecimalScale("3." + "3".repeat(64) + "B * 2.0B", new BigDecimal("6." + "6".repeat(64) + "0"));
        testBigDecimalScale("2.0B * 3." + "3".repeat(64) + "B", new BigDecimal("6." + "6".repeat(64) + "0")); // swap operands

        // left division
        testBigDecimalScale("9.0B / 3.0B", new BigDecimal("3.00000000000000000000000000000000"));
        testBigDecimalScale("9.999999B / 3.0B", new BigDecimal("3.33333300000000000000000000000000"));
        testBigDecimalScale("9.99999999999999999999B / 3.0B", new BigDecimal("3.33333333333333333333000000000000"));
        testBigDecimalScale("9." + "9".repeat(32) + "B / 3.0B", new BigDecimal("3." + "3".repeat(32)));
        testBigDecimalScale("9." + "9".repeat(64) + "B / 3.0B", new BigDecimal("3." + "3".repeat(64)));
        testBigDecimalScale("3.0B / 9." + "9".repeat(64) + "B", new BigDecimal("0.3" + "0".repeat(63))); // swap operands

        // right division
        testBigDecimalScale("3.0B \\ 9.0B", new BigDecimal("3.00000000000000000000000000000000"));
        testBigDecimalScale("3.0B \\ 9.999999B", new BigDecimal("3.33333300000000000000000000000000"));
        testBigDecimalScale("3.0B \\ 9.99999999999999999999B", new BigDecimal("3.33333333333333333333000000000000"));
        testBigDecimalScale("3.0B \\ 9." + "9".repeat(32) + "B", new BigDecimal("3." + "3".repeat(32)));
        testBigDecimalScale("3.0B \\ 9." + "9".repeat(64) + "B", new BigDecimal("3." + "3".repeat(64)));
        testBigDecimalScale("9." + "9".repeat(64) + "B \\ 3.0B", new BigDecimal("0.3" + "0".repeat(63))); // swap operands
    }
}
