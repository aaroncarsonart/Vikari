package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static com.atonementcrystals.dnr.vikari.TestUtils.testNumberCrystal;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_Arithmetic {
    private final MathContext mathContext = Arithmetic.getMathContext();

    /**
     * Helper method to efficiently test a single Vikari expression statement returning a numeric value.
     * @param sourceString The Vikari source code to execute.
     * @param expectedValue The expected value for the result.
     * @param expectedClass The expected type of the result.
     */
    private void testVikariExpression(String sourceString, Object expectedValue,
                                            Class<? extends NumberCrystal<?>> expectedClass) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);
        assertEquals(1, parsedStatements.size(), "Unexpected number of statements");

        // statement 1
        Statement statement = parsedStatements.get(0);
        AtonementCrystal crystal = interpreter.execute(statement);
        testNumberCrystal(crystal, expectedValue, expectedClass);
    }

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_ExpressionStatement_IntegerLiteral() {
        testVikariExpression("22", 22, IntegerCrystal.class);
        testVikariExpression("0", 0, IntegerCrystal.class);
        testVikariExpression(String.valueOf(Integer.MAX_VALUE), Integer.MAX_VALUE, IntegerCrystal.class);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_ExpressionStatement_LongLiteral() {
        testVikariExpression("400000000000", 400000000000L, LongCrystal.class);
        testVikariExpression("22L", 22L, LongCrystal.class);
        testVikariExpression("7l", 7l, LongCrystal.class);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_ExpressionStatement_BigIntegerLiteral() {
        String largeNumber = "10000000000000000000";
        testVikariExpression(largeNumber, new BigInteger(largeNumber), BigIntegerCrystal.class);
        testVikariExpression("512B", new BigInteger("512"), BigIntegerCrystal.class);
        testVikariExpression("1024b", new BigInteger("1024"), BigIntegerCrystal.class);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_ExpressionStatement_FloatLiteral() {
        testVikariExpression("22F", 22F, FloatCrystal.class);
        testVikariExpression("7f", 7f, FloatCrystal.class);
        testVikariExpression("3.14F", 3.14F, FloatCrystal.class);
        testVikariExpression("6.28f", 6.28f, FloatCrystal.class);
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_ExpressionStatement_DoubleLiteral() {
        testVikariExpression("22D", 22D, DoubleCrystal.class);
        testVikariExpression("7d", 7d, DoubleCrystal.class);
        testVikariExpression("3.14D", 3.14D, DoubleCrystal.class);
        testVikariExpression("6.28d", 6.28d, DoubleCrystal.class);
        testVikariExpression("999.999", 999.999, DoubleCrystal.class);
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_ExpressionStatement_BigDecimalLiteral() {
        testVikariExpression("3.14B", new BigDecimal("3.14", mathContext), BigDecimalCrystal.class);
        testVikariExpression("6.28b", new BigDecimal("6.28", mathContext), BigDecimalCrystal.class);
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_ExpressionStatement_IntegerLiteral_Negated() {
        testVikariExpression("-22", -22, IntegerCrystal.class);
        testVikariExpression(String.valueOf(Integer.MIN_VALUE), Integer.MIN_VALUE, IntegerCrystal.class);
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_ExpressionStatement_LongLiteral_Negated() {
        testVikariExpression("-400000000000", -400000000000L, LongCrystal.class);
        testVikariExpression("-22L", -22L, LongCrystal.class);
        testVikariExpression("-7l", -7l, LongCrystal.class);
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_ExpressionStatement_BigIntegerLiteral_Negated() {
        String largeNumber = "-10000000000000000000";
        testVikariExpression(largeNumber, new BigInteger(largeNumber), BigIntegerCrystal.class);
        testVikariExpression("-512B", new BigInteger("-512"), BigIntegerCrystal.class);
        testVikariExpression("-1024b", new BigInteger("-1024"), BigIntegerCrystal.class);
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_ExpressionStatement_FloatLiteral_Negated() {
        testVikariExpression("-22F", -22F, FloatCrystal.class);
        testVikariExpression("-7f", -7f, FloatCrystal.class);
        testVikariExpression("-3.14F", -3.14F, FloatCrystal.class);
        testVikariExpression("-6.28f", -6.28f, FloatCrystal.class);
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_ExpressionStatement_DoubleLiteral_Negated() {
        testVikariExpression("-22D", -22D, DoubleCrystal.class);
        testVikariExpression("-7d", -7d, DoubleCrystal.class);
        testVikariExpression("-3.14D", -3.14D, DoubleCrystal.class);
        testVikariExpression("-6.28d", -6.28d, DoubleCrystal.class);
        testVikariExpression("-999.999", -999.999, DoubleCrystal.class);
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_ExpressionStatement_BigDecimalLiteral_Negated() {
        testVikariExpression("-3.14B", new BigDecimal("-3.14", mathContext), BigDecimalCrystal.class);
        testVikariExpression("-6.28b", new BigDecimal("-6.28", mathContext), BigDecimalCrystal.class);
    }

    @Test
    @Order(13)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_IntegerResult() {
        testVikariExpression("7 + 22", 7 + 22, IntegerCrystal.class);
        testVikariExpression("7 - 22", 7 - 22, IntegerCrystal.class);
        testVikariExpression("7 * 22", 7 * 22, IntegerCrystal.class);
        testVikariExpression("7 / 22", 7 / 22, IntegerCrystal.class);
        testVikariExpression("7 \\ 22", 22 / 7, IntegerCrystal.class);
        // with grouping
        testVikariExpression("[7 + 22]", (7 + 22), IntegerCrystal.class);
        testVikariExpression("[7 - 22]", (7 - 22), IntegerCrystal.class);
        testVikariExpression("[7 * 22]", (7 * 22), IntegerCrystal.class);
        testVikariExpression("[7 / 22]", (7 / 22), IntegerCrystal.class);
        testVikariExpression("[7 \\ 22]", (22 / 7), IntegerCrystal.class);
        // negated
        testVikariExpression("-[7 + 22]", -(7 + 22), IntegerCrystal.class);
        testVikariExpression("-[7 - 22]", -(7 - 22), IntegerCrystal.class);
        testVikariExpression("-[7 * 22]", -(7 * 22), IntegerCrystal.class);
        testVikariExpression("-[7 / 22]", -(7 / 22), IntegerCrystal.class);
        testVikariExpression("-[7 \\ 22]", -(22 / 7), IntegerCrystal.class);
        // complex expression
        testVikariExpression("7 + [22 - 3] / [17 * 32]", 7 + (22 - 3) / (17 * 32), IntegerCrystal.class);
    }

    @Test
    @Order(14)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_LongResult() {
        testVikariExpression("7L + 22L", 7L + 22L, LongCrystal.class);
        testVikariExpression("7L - 22L", 7L - 22L, LongCrystal.class);
        testVikariExpression("7L * 22L", 7L * 22L, LongCrystal.class);
        testVikariExpression("7L / 22L", 7L / 22L, LongCrystal.class);
        testVikariExpression("7L \\ 22L", 22L / 7L, LongCrystal.class);
        // with grouping
        testVikariExpression("[7L + 22L]", (7L + 22L), LongCrystal.class);
        testVikariExpression("[7L - 22L]", (7L - 22L), LongCrystal.class);
        testVikariExpression("[7L * 22L]", (7L * 22L), LongCrystal.class);
        testVikariExpression("[7L / 22L]", (7L / 22L), LongCrystal.class);
        testVikariExpression("[7L \\ 22L]", (22L / 7L), LongCrystal.class);
        // negated
        testVikariExpression("-[7L + 22L]", -(7L + 22L), LongCrystal.class);
        testVikariExpression("-[7L - 22L]", -(7L - 22L), LongCrystal.class);
        testVikariExpression("-[7L * 22L]", -(7L * 22L), LongCrystal.class);
        testVikariExpression("-[7L / 22L]", -(7L / 22L), LongCrystal.class);
        testVikariExpression("-[7L \\ 22L]", -(22L / 7L), LongCrystal.class);
        // complex expression
        testVikariExpression("7L + [22L - 3L] / [17L * 32L]", 7L + (22L - 3L) / (17L * 32L), LongCrystal.class);
    }

    @Test
    @Order(15)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_BigIntegerResult() {
        testVikariExpression("7B + 22B", BigInteger.valueOf(7L + 22L), BigIntegerCrystal.class);
        testVikariExpression("7B - 22B", BigInteger.valueOf(7L - 22L), BigIntegerCrystal.class);
        testVikariExpression("7B * 22B", BigInteger.valueOf(7L * 22L), BigIntegerCrystal.class);
        testVikariExpression("7B / 22B", BigInteger.valueOf(7L / 22L), BigIntegerCrystal.class);
        testVikariExpression("7B \\ 22B", BigInteger.valueOf(22L / 7L), BigIntegerCrystal.class);
        // with grouping
        testVikariExpression("[7B + 22B]", BigInteger.valueOf(7L + 22L), BigIntegerCrystal.class);
        testVikariExpression("[7B - 22B]", BigInteger.valueOf(7L - 22L), BigIntegerCrystal.class);
        testVikariExpression("[7B * 22B]", BigInteger.valueOf(7L * 22L), BigIntegerCrystal.class);
        testVikariExpression("[7B / 22B]", BigInteger.valueOf(7L / 22L), BigIntegerCrystal.class);
        testVikariExpression("[7B \\ 22B]", BigInteger.valueOf(22L / 7L), BigIntegerCrystal.class);
        // negated
        testVikariExpression("-[7B + 22B]", BigInteger.valueOf(7L + 22L).negate(), BigIntegerCrystal.class);
        testVikariExpression("-[7B - 22B]", BigInteger.valueOf(7L - 22L).negate(), BigIntegerCrystal.class);
        testVikariExpression("-[7B * 22B]", BigInteger.valueOf(7L * 22L).negate(), BigIntegerCrystal.class);
        testVikariExpression("-[7B / 22B]", BigInteger.valueOf(7L / 22L).negate(), BigIntegerCrystal.class);
        testVikariExpression("-[7B \\ 22B]", BigInteger.valueOf(22L / 7L).negate(), BigIntegerCrystal.class);
        // complex expression
        testVikariExpression("7B + [22B - 3B] / [17B * 32B]", BigInteger.valueOf(7L + (22L - 3L) / (17L * 32L)), BigIntegerCrystal.class);
    }

    @Test
    @Order(16)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_FloatResult_NoFractionalPart() {
        testVikariExpression("7F + 22F", 7F + 22F, FloatCrystal.class);
        testVikariExpression("7F - 22F", 7F - 22F, FloatCrystal.class);
        testVikariExpression("7F * 22F", 7F * 22F, FloatCrystal.class);
        testVikariExpression("7F / 22F", 7F / 22F, FloatCrystal.class);
        testVikariExpression("7F \\ 22F", 22F / 7F, FloatCrystal.class);
        // with grouping
        testVikariExpression("[7F + 22F]", (7F + 22F), FloatCrystal.class);
        testVikariExpression("[7F - 22F]", (7F - 22F), FloatCrystal.class);
        testVikariExpression("[7F * 22F]", (7F * 22F), FloatCrystal.class);
        testVikariExpression("[7F / 22F]", (7F / 22F), FloatCrystal.class);
        testVikariExpression("[7F \\ 22F]", (22F / 7F), FloatCrystal.class);
        // negated
        testVikariExpression("-[7F + 22F]", -(7F + 22F), FloatCrystal.class);
        testVikariExpression("-[7F - 22F]", -(7F - 22F), FloatCrystal.class);
        testVikariExpression("-[7F * 22F]", -(7F * 22F), FloatCrystal.class);
        testVikariExpression("-[7F / 22F]", -(7F / 22F), FloatCrystal.class);
        testVikariExpression("-[7F \\ 22F]", -(22F / 7F), FloatCrystal.class);
        // complex expression
        testVikariExpression("7F + [22F - 3F] / [17F * 32F]", 7F + (22F - 3F) / (17F * 32F), FloatCrystal.class);
    }

    @Test
    @Order(17)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_DoubleResult_NoFractionalPart() {
        testVikariExpression("7F + 22D", 7D + 22D, DoubleCrystal.class);
        testVikariExpression("7D - 22D", 7D - 22D, DoubleCrystal.class);
        testVikariExpression("7D * 22D", 7D * 22D, DoubleCrystal.class);
        testVikariExpression("7D / 22D", 7D / 22D, DoubleCrystal.class);
        testVikariExpression("7D \\ 22D", 22D / 7D, DoubleCrystal.class);
        // with grouping
        testVikariExpression("[7D + 22D]", (7D + 22D), DoubleCrystal.class);
        testVikariExpression("[7D - 22D]", (7D - 22D), DoubleCrystal.class);
        testVikariExpression("[7D * 22D]", (7D * 22D), DoubleCrystal.class);
        testVikariExpression("[7D / 22D]", (7D / 22D), DoubleCrystal.class);
        testVikariExpression("[7D \\ 22D]", (22D / 7D), DoubleCrystal.class);
        // negated
        testVikariExpression("-[7D + 22D]", -(7D + 22D), DoubleCrystal.class);
        testVikariExpression("-[7D - 22D]", -(7D - 22D), DoubleCrystal.class);
        testVikariExpression("-[7D * 22D]", -(7D * 22D), DoubleCrystal.class);
        testVikariExpression("-[7D / 22D]", -(7D / 22D), DoubleCrystal.class);
        testVikariExpression("-[7D \\ 22D]", -(22D / 7D), DoubleCrystal.class);
        // complex expression
        testVikariExpression("7D + [22D - 3D] / [17D * 32D]", 7D + (22D - 3D) / (17D * 32D), DoubleCrystal.class);
    }

    @Test
    @Order(18)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_FloatResult_WithFractionalPart() {
        testVikariExpression("22.7F + 3.14F", 22.7F + 3.14F, FloatCrystal.class);
        testVikariExpression("22.7F - 3.14F", 22.7F - 3.14F, FloatCrystal.class);
        testVikariExpression("22.7F * 3.14F", 22.7F * 3.14F, FloatCrystal.class);
        testVikariExpression("22.7F / 3.14F", 22.7F / 3.14F, FloatCrystal.class);
        testVikariExpression("22.7F \\ 3.14F", 3.14F / 22.7F, FloatCrystal.class);
        // with grouping
        testVikariExpression("[22.7F + 3.14F]", (22.7F + 3.14F), FloatCrystal.class);
        testVikariExpression("[22.7F - 3.14F]", (22.7F - 3.14F), FloatCrystal.class);
        testVikariExpression("[22.7F * 3.14F]", (22.7F * 3.14F), FloatCrystal.class);
        testVikariExpression("[22.7F / 3.14F]", (22.7F / 3.14F), FloatCrystal.class);
        testVikariExpression("[22.7F \\ 3.14F]", (3.14F / 22.7F), FloatCrystal.class);
        // negated
        testVikariExpression("-[22.7F + 3.14F]", -(22.7F + 3.14F), FloatCrystal.class);
        testVikariExpression("-[22.7F - 3.14F]", -(22.7F - 3.14F), FloatCrystal.class);
        testVikariExpression("-[22.7F * 3.14F]", -(22.7F * 3.14F), FloatCrystal.class);
        testVikariExpression("-[22.7F / 3.14F]", -(22.7F / 3.14F), FloatCrystal.class);
        testVikariExpression("-[22.7F \\ 3.14F]", -(3.14F / 22.7F), FloatCrystal.class);
        // complex expression
        testVikariExpression("22.7F + [17.4F - 0.3F] / [0.17F * 32.9F]", 22.7F + (17.4F - 0.3F) / (0.17F * 32.9F), FloatCrystal.class);
    }

    @Test
    @Order(19)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_DoubleResult_WithFractionalPart() {
        testVikariExpression("22.7D + 3.14D", 22.7D + 3.14D, DoubleCrystal.class);
        testVikariExpression("22.7D - 3.14D", 22.7D - 3.14D, DoubleCrystal.class);
        testVikariExpression("22.7D * 3.14D", 22.7D * 3.14D, DoubleCrystal.class);
        testVikariExpression("22.7D / 3.14D", 22.7D / 3.14D, DoubleCrystal.class);
        testVikariExpression("22.7D \\ 3.14D", 3.14D / 22.7D, DoubleCrystal.class);
        // with grouping
        testVikariExpression("[22.7D + 3.14D]", (22.7D + 3.14D), DoubleCrystal.class);
        testVikariExpression("[22.7D - 3.14D]", (22.7D - 3.14D), DoubleCrystal.class);
        testVikariExpression("[22.7D * 3.14D]", (22.7D * 3.14D), DoubleCrystal.class);
        testVikariExpression("[22.7D / 3.14D]", (22.7D / 3.14D), DoubleCrystal.class);
        testVikariExpression("[22.7D \\ 3.14D]", (3.14D / 22.7D), DoubleCrystal.class);
        // negated
        testVikariExpression("-[22.7D + 3.14D]", -(22.7D + 3.14D), DoubleCrystal.class);
        testVikariExpression("-[22.7D - 3.14D]", -(22.7D - 3.14D), DoubleCrystal.class);
        testVikariExpression("-[22.7D * 3.14D]", -(22.7D * 3.14D), DoubleCrystal.class);
        testVikariExpression("-[22.7D / 3.14D]", -(22.7D / 3.14D), DoubleCrystal.class);
        testVikariExpression("-[22.7D \\ 3.14D]", -(3.14D / 22.7D), DoubleCrystal.class);
        // complex expression
        testVikariExpression("22.7D + [17.4D - 0.3D] / [0.17D * 32.9D]", 22.7D + (17.4D - 0.3D) / (0.17D * 32.9D), DoubleCrystal.class);
    }

    @Test
    @Order(20)
    public void testTreeWalkInterpreter_ExpressionStatement_BinaryExpression_BigDecimalResult_WithFractionalPart() {
        Class<BigDecimalCrystal> expectedClass = BigDecimalCrystal.class;
        BigDecimal firstOperand = new BigDecimal("22.7", mathContext);
        BigDecimal secondOperand = new BigDecimal("3.14", mathContext);

        // add
        String sourceString = "22.7B + 3.14B";
        BigDecimal expectedValue = firstOperand.add(secondOperand, mathContext);
        testVikariExpression(sourceString, expectedValue, expectedClass);
        testVikariExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testVikariExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // subtract
        sourceString = "22.7B - 3.14B";
        expectedValue = firstOperand.subtract(secondOperand, mathContext);
        testVikariExpression(sourceString, expectedValue, expectedClass);
        testVikariExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testVikariExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // multiply
        sourceString = "22.7B * 3.14B";
        expectedValue = firstOperand.multiply(secondOperand, mathContext);
        testVikariExpression(sourceString, expectedValue, expectedClass);
        testVikariExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testVikariExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // left divide
        sourceString = "22.7B / 3.14B";
        expectedValue = firstOperand.divide(secondOperand, mathContext);
        testVikariExpression(sourceString, expectedValue, expectedClass);
        testVikariExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testVikariExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // right divide
        sourceString = "22.7B \\ 3.14B";
        expectedValue = secondOperand.divide(firstOperand, mathContext);
        testVikariExpression(sourceString, expectedValue, expectedClass);
        testVikariExpression("[" + sourceString + "]", expectedValue, expectedClass);
        testVikariExpression("-[" + sourceString + "]", expectedValue.negate(), expectedClass);

        // complex expression
        BigDecimal thirdOperand, fourthOperand, fifthOperand;

        secondOperand = new BigDecimal("17.4", mathContext);
        thirdOperand = new BigDecimal("0.3", mathContext);
        fourthOperand = new BigDecimal("0.17", mathContext);
        fifthOperand = new BigDecimal("32.9", mathContext);

        sourceString = "22.7B + [17.4B - 0.3B] / [0.17B * 32.9B]";

        BigDecimal divisor = secondOperand.subtract(thirdOperand, mathContext);
        BigDecimal dividend = fourthOperand.multiply(fifthOperand, mathContext);
        expectedValue = firstOperand.add(divisor.divide(dividend, mathContext), mathContext);

        testVikariExpression(sourceString, expectedValue, expectedClass);
    }

    @Test
    @Order(21)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_IntToLong() {
        testVikariExpression("22 + 7L", 29L, LongCrystal.class);
        testVikariExpression("22L + 7", 29L, LongCrystal.class);
        testVikariExpression("22L - 7", 15L, LongCrystal.class);
        testVikariExpression("22 - 7L", 15L, LongCrystal.class);
        testVikariExpression("22L * 7", 154L, LongCrystal.class);
        testVikariExpression("22 * 7L", 154L, LongCrystal.class);
        testVikariExpression("22L / 7", 3L, LongCrystal.class);
        testVikariExpression("22 / 7L", 3L, LongCrystal.class);
        testVikariExpression("22L \\ 7", 0L, LongCrystal.class);
        testVikariExpression("22 \\ 7L", 0L, LongCrystal.class);
        testVikariExpression("2147483647 + 1", 2147483648L, LongCrystal.class);
        testVikariExpression("-2147483648 - 1", -2147483649L, LongCrystal.class);
        testVikariExpression("2147483647 * 2", 4294967294L, LongCrystal.class);
    }

    @Test
    @Order(22)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_LongToBigInt() {
        testVikariExpression("22L + 7B", new BigInteger("29"), BigIntegerCrystal.class);
        testVikariExpression("22B + 7L", new BigInteger("29"), BigIntegerCrystal.class);
        testVikariExpression("22L - 7B", new BigInteger("15"), BigIntegerCrystal.class);
        testVikariExpression("22B - 7L", new BigInteger("15"), BigIntegerCrystal.class);
        testVikariExpression("22L * 7B", new BigInteger("154"), BigIntegerCrystal.class);
        testVikariExpression("22B * 7L", new BigInteger("154"), BigIntegerCrystal.class);
        testVikariExpression("22L / 7B", new BigInteger("3"), BigIntegerCrystal.class);
        testVikariExpression("22B / 7L", new BigInteger("3"), BigIntegerCrystal.class);
        testVikariExpression("22L \\ 7B", new BigInteger("0"), BigIntegerCrystal.class);
        testVikariExpression("22B \\ 7L", new BigInteger("0"), BigIntegerCrystal.class);

        testVikariExpression("9223372036854775807 + 1L", new BigInteger("9223372036854775808"), BigIntegerCrystal.class);
        testVikariExpression("-9223372036854775808 - 1L", new BigInteger("-9223372036854775809"), BigIntegerCrystal.class);
        testVikariExpression("9223372036854775807 * 2L", new BigInteger("18446744073709551614"), BigIntegerCrystal.class);
    }

    @Test
    @Order(23)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_IntToBigInt() {
        testVikariExpression("22 + 7B", new BigInteger("29"), BigIntegerCrystal.class);
        testVikariExpression("22B + 7", new BigInteger("29"), BigIntegerCrystal.class);
        testVikariExpression("22 - 7B", new BigInteger("15"), BigIntegerCrystal.class);
        testVikariExpression("22B - 7", new BigInteger("15"), BigIntegerCrystal.class);
        testVikariExpression("22 * 7B", new BigInteger("154"), BigIntegerCrystal.class);
        testVikariExpression("22B * 7", new BigInteger("154"), BigIntegerCrystal.class);
        testVikariExpression("22 / 7B", new BigInteger("3"), BigIntegerCrystal.class);
        testVikariExpression("22B / 7", new BigInteger("3"), BigIntegerCrystal.class);
        testVikariExpression("22 \\ 7B", new BigInteger("0"), BigIntegerCrystal.class);
        testVikariExpression("22B \\ 7", new BigInteger("0"), BigIntegerCrystal.class);

        testVikariExpression("9223372036854775807 + 1", new BigInteger("9223372036854775808"), BigIntegerCrystal.class);
        testVikariExpression("-9223372036854775808 - 1", new BigInteger("-9223372036854775809"), BigIntegerCrystal.class);
        testVikariExpression("9223372036854775807 * 2", new BigInteger("18446744073709551614"), BigIntegerCrystal.class);

        testVikariExpression("2147483647 * 2147483647 * 3", new BigInteger("13835058042397261827"), BigIntegerCrystal.class);
        testVikariExpression("-[2147483647 * 2147483647 * 3]", new BigInteger("-13835058042397261827"), BigIntegerCrystal.class);
    }

    @Test
    @Order(24)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_FloatToDouble() {
        testVikariExpression("3.14F + 6.28D", 9.42D, DoubleCrystal.class);
        testVikariExpression("3.14D + 6.28F", 9.42D, DoubleCrystal.class);
        testVikariExpression("3.14F - 6.28D", -3.14D, DoubleCrystal.class);
        testVikariExpression("3.14D - 6.28F", -3.14D, DoubleCrystal.class);
        testVikariExpression("3.14F * 6.28D", 19.7192D, DoubleCrystal.class);
        testVikariExpression("3.14D * 6.28F", 19.7192D, DoubleCrystal.class);
        testVikariExpression("3.14F / 6.28D", 0.5D, DoubleCrystal.class);
        testVikariExpression("3.14D / 6.28F", 0.5D, DoubleCrystal.class);
        testVikariExpression("3.14F \\ 6.28D", 2.0D, DoubleCrystal.class);
        testVikariExpression("3.14D \\ 6.28F", 2.0D, DoubleCrystal.class);
    }

    @Test
    @Order(25)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_FloatToBigDecimal() {
        testVikariExpression("3.14F + 6.28B", new BigDecimal("9.42", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B + 6.28F", new BigDecimal("9.42", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14F - 6.28B", new BigDecimal("-3.14", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B - 6.28F", new BigDecimal("-3.14", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14F * 6.28B", new BigDecimal("19.7192", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B * 6.28F", new BigDecimal("19.7192", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14F / 6.28B", new BigDecimal("0.5", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B / 6.28F", new BigDecimal("0.5", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14F \\ 6.28B", new BigDecimal("2", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B \\ 6.28F", new BigDecimal("2", mathContext), BigDecimalCrystal.class);
    }

    @Test
    @Order(26)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_DoubleToBigDecimal() {
        testVikariExpression("3.14D + 6.28B", new BigDecimal("9.42", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B + 6.28D", new BigDecimal("9.42", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14D - 6.28B", new BigDecimal("-3.14", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B - 6.28D", new BigDecimal("-3.14", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14D * 6.28B", new BigDecimal("19.7192", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B * 6.28D", new BigDecimal("19.7192", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14D / 6.28B", new BigDecimal("0.5", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B / 6.28D", new BigDecimal("0.5", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14D \\ 6.28B", new BigDecimal("2", mathContext), BigDecimalCrystal.class);
        testVikariExpression("3.14B \\ 6.28D", new BigDecimal("2", mathContext), BigDecimalCrystal.class);
    }

    @Test
    @Order(27)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_IntToFloat() {
        testVikariExpression("22F + 7", 29F, FloatCrystal.class);
        testVikariExpression("22 + 7F", 29F, FloatCrystal.class);
        testVikariExpression("22F - 7", 15F, FloatCrystal.class);
        testVikariExpression("22 - 7F", 15F, FloatCrystal.class);
        testVikariExpression("22F * 7", 154F, FloatCrystal.class);
        testVikariExpression("22 * 7F", 154F, FloatCrystal.class);
        testVikariExpression("22F / 7", 3.142857F, FloatCrystal.class);
        testVikariExpression("22 / 7F", 3.142857F, FloatCrystal.class);
        testVikariExpression("22F \\ 7", 0.3181818F, FloatCrystal.class);
        testVikariExpression("22 \\ 7F", 0.3181818F, FloatCrystal.class);
    }

    @Test
    @Order(28)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_LongToFloat() {
        testVikariExpression("22F + 7L", 29F, FloatCrystal.class);
        testVikariExpression("22L + 7F", 29F, FloatCrystal.class);
        testVikariExpression("22F - 7L", 15F, FloatCrystal.class);
        testVikariExpression("22L - 7F", 15F, FloatCrystal.class);
        testVikariExpression("22F * 7", 154F, FloatCrystal.class);
        testVikariExpression("22L * 7F", 154F, FloatCrystal.class);
        testVikariExpression("22F / 7L", 3.142857F, FloatCrystal.class);
        testVikariExpression("22L / 7F", 3.142857F, FloatCrystal.class);
        testVikariExpression("22F \\ 7L", 0.3181818F, FloatCrystal.class);
        testVikariExpression("22L \\ 7F", 0.3181818F, FloatCrystal.class);
    }

    @Test
    @Order(29)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_BigIntToFloat() {
        testVikariExpression("22F + 7B", 29F, FloatCrystal.class);
        testVikariExpression("22B + 7F", 29F, FloatCrystal.class);
        testVikariExpression("22F - 7B", 15F, FloatCrystal.class);
        testVikariExpression("22B - 7F", 15F, FloatCrystal.class);
        testVikariExpression("22F * 7B", 154F, FloatCrystal.class);
        testVikariExpression("22B * 7F", 154F, FloatCrystal.class);
        testVikariExpression("22F / 7B", 3.142857F, FloatCrystal.class);
        testVikariExpression("22B / 7F", 3.142857F, FloatCrystal.class);
        testVikariExpression("22F \\ 7B", 0.3181818F, FloatCrystal.class);
        testVikariExpression("22B \\ 7F", 0.3181818F, FloatCrystal.class);
    }

    @Test
    @Order(30)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_IntToDouble() {
        testVikariExpression("22D + 7", 29D, DoubleCrystal.class);
        testVikariExpression("22 + 7D", 29D, DoubleCrystal.class);
        testVikariExpression("22D - 7", 15D, DoubleCrystal.class);
        testVikariExpression("22 - 7D", 15D, DoubleCrystal.class);
        testVikariExpression("22D * 7", 154D, DoubleCrystal.class);
        testVikariExpression("22 * 7D", 154D, DoubleCrystal.class);
        testVikariExpression("22D / 7", 3.142857142857143D, DoubleCrystal.class);
        testVikariExpression("22 / 7D", 3.142857142857143D, DoubleCrystal.class);
        testVikariExpression("22D \\ 7", 0.3181818181818182D, DoubleCrystal.class);
        testVikariExpression("22 \\ 7D", 0.3181818181818182D, DoubleCrystal.class);
    }

    @Test
    @Order(31)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_LongToDouble() {
        testVikariExpression("22D + 7L", 29D, DoubleCrystal.class);
        testVikariExpression("22L + 7D", 29D, DoubleCrystal.class);
        testVikariExpression("22D - 7L", 15D, DoubleCrystal.class);
        testVikariExpression("22L - 7D", 15D, DoubleCrystal.class);
        testVikariExpression("22D * 7", 154D, DoubleCrystal.class);
        testVikariExpression("22L * 7D", 154D, DoubleCrystal.class);
        testVikariExpression("22D / 7L", 3.142857142857143D, DoubleCrystal.class);
        testVikariExpression("22L / 7D", 3.142857142857143D, DoubleCrystal.class);
        testVikariExpression("22D \\ 7L", 0.3181818181818182D, DoubleCrystal.class);
        testVikariExpression("22L \\ 7D", 0.3181818181818182D, DoubleCrystal.class);
    }

    @Test
    @Order(32)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_BigIntToDouble() {
        testVikariExpression("22D + 7B", 29D, DoubleCrystal.class);
        testVikariExpression("22B + 7D", 29D, DoubleCrystal.class);
        testVikariExpression("22D - 7B", 15D, DoubleCrystal.class);
        testVikariExpression("22B - 7D", 15D, DoubleCrystal.class);
        testVikariExpression("22D * 7B", 154D, DoubleCrystal.class);
        testVikariExpression("22B * 7D", 154D, DoubleCrystal.class);
        testVikariExpression("22D / 7B", 3.142857142857143D, DoubleCrystal.class);
        testVikariExpression("22B / 7D", 3.142857142857143D, DoubleCrystal.class);
        testVikariExpression("22D \\ 7B", 0.3181818181818182D, DoubleCrystal.class);
        testVikariExpression("22B \\ 7D", 0.3181818181818182D, DoubleCrystal.class);
    }

    @Test
    @Order(33)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_IntToBigDecimal() {
        testVikariExpression("22.0B + 7", new BigDecimal("29.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22 + 7.0B", new BigDecimal("29.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22.0B - 7", new BigDecimal("15.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22 - 7.0B", new BigDecimal("15.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22.0B * 7", new BigDecimal("154.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22 * 7.0B", new BigDecimal("154.0", mathContext), BigDecimalCrystal.class);

        BigDecimal expectedResult = new BigDecimal("3.142857142857142857142857142857143", mathContext);
        testVikariExpression("22.0B / 7", expectedResult, BigDecimalCrystal.class);
        testVikariExpression("22 / 7.0B", expectedResult, BigDecimalCrystal.class);

        expectedResult = new BigDecimal("0.3181818181818181818181818181818182", mathContext);
        testVikariExpression("22.0B \\ 7", expectedResult, BigDecimalCrystal.class);
        testVikariExpression("22 \\ 7.0B", expectedResult, BigDecimalCrystal.class);
    }

    @Test
    @Order(34)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_LongToBigDecimal() {
        testVikariExpression("22.0B + 7L", new BigDecimal("29.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22L + 7.0B", new BigDecimal("29.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22.0B - 7L", new BigDecimal("15.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22L - 7.0B", new BigDecimal("15.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22.0B * 7", new BigDecimal("154.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22L * 7.0B", new BigDecimal("154.0", mathContext), BigDecimalCrystal.class);

        BigDecimal expectedResult = new BigDecimal("3.142857142857142857142857142857143", mathContext);
        testVikariExpression("22.0B / 7L", expectedResult, BigDecimalCrystal.class);
        testVikariExpression("22L / 7.0B", expectedResult, BigDecimalCrystal.class);

        expectedResult = new BigDecimal("0.3181818181818181818181818181818182", mathContext);
        testVikariExpression("22.0B \\ 7L", expectedResult, BigDecimalCrystal.class);
        testVikariExpression("22L \\ 7.0B", expectedResult, BigDecimalCrystal.class);
    }

    @Test
    @Order(35)
    public void testTreeWalkInterpreter_PromotionOfNumericTypes_BigIntToBigDecimal() {
        testVikariExpression("22.0B + 7B", new BigDecimal("29.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22B + 7.0B", new BigDecimal("29.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22.0B - 7B", new BigDecimal("15.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22B - 7.0B", new BigDecimal("15.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22.0B * 7B", new BigDecimal("154.0", mathContext), BigDecimalCrystal.class);
        testVikariExpression("22B * 7.0B", new BigDecimal("154.0", mathContext), BigDecimalCrystal.class);

        BigDecimal expectedResult = new BigDecimal("3.142857142857142857142857142857143", mathContext);
        testVikariExpression("22.0B / 7B", expectedResult, BigDecimalCrystal.class);
        testVikariExpression("22B / 7.0B", expectedResult, BigDecimalCrystal.class);

        expectedResult = new BigDecimal("0.3181818181818181818181818181818182", mathContext);
        testVikariExpression("22.0B \\ 7B", expectedResult, BigDecimalCrystal.class);
        testVikariExpression("22B \\ 7.0B", expectedResult, BigDecimalCrystal.class);
    }
}
