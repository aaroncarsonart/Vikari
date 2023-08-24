package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.lexSingleStatement;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_Numbers {
    private void lexAndTestNumberLiteral(String sourceString, Class<? extends AtonementCrystal> expectedClass,
                                         Object expectedValue) {

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        if (statement.get(0) instanceof NumberCrystal<?> numberCrystal) {
            boolean isNegative = numberCrystal.getNegationOperatorLocation() != null;

            CoordinatePair expectedNumberLocation = isNegative ? location(0, 1) : location(0, 0);
            testCrystal(numberCrystal, expectedClass, sourceString, expectedNumberLocation);
            assertEquals(expectedValue, numberCrystal.getValue(), "Unexpected value for NumberCrystal.");

            if (isNegative) {
                testNegationOperatorLocation(numberCrystal, location(0, 0));
            }
        } else {
            fail("Expected result to be of type NumberCrystal.");
        }
    }

    private void lexAndTestNumberLiteral_OverflowError(String sourceString,
                                                       Class<? extends AtonementCrystal> expectedClass,
                                                       Object expectedValue) {

        if (expectedValue instanceof BigInteger || expectedValue instanceof BigDecimal) {
            fail("Malformed test. Overflow errors do not exist for BigInteger and BigDecimal literals.");
        }

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1, errorReporter, 1);
        AtonementCrystal crystal = statement.get(0);

        boolean isNegative = false;
        if (crystal instanceof NumberCrystal<?> numberCrystal) {
            isNegative = numberCrystal.getNegationOperatorLocation() != null;
            CoordinatePair expectedLocation = isNegative ? location(0, 1) : location(0, 0);

            testCrystal(numberCrystal, expectedClass, sourceString, expectedLocation);
            assertEquals(expectedValue, numberCrystal.getValue(), "Unexpected value for NumberCrystal.");
        } else {
            fail("Expected result to be of type NumberCrystal.");
        }

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        CoordinatePair expectedLocation = crystal.getCoordinates();

        String numberTypeName = expectedValue.getClass().getSimpleName();
        String smallOrLarge = isNegative ? "small" : "large";
        String expectedMessage = numberTypeName + " literal value is too " + smallOrLarge + ".";

        testSyntaxError(syntaxErrors.get(0), expectedLocation, sourceString, expectedMessage);
    }

    @Test
    @Order(1)
    public void testLexer_Crystals_Numbers_IntegerLiterals() {
        lexAndTestNumberLiteral("1", IntegerCrystal.class, 1);
        lexAndTestNumberLiteral("5I", IntegerCrystal.class, 5);
        lexAndTestNumberLiteral("22i", IntegerCrystal.class, 22);

        lexAndTestNumberLiteral("-1", IntegerCrystal.class, -1);
        lexAndTestNumberLiteral("-5I", IntegerCrystal.class, -5);
        lexAndTestNumberLiteral("-22i", IntegerCrystal.class, -22);
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_Numbers_LongLiterals() {
        lexAndTestNumberLiteral("4000000000", LongCrystal.class, 4000000000L);
        lexAndTestNumberLiteral("22L", LongCrystal.class, 22L);
        lexAndTestNumberLiteral("47l", LongCrystal.class, 47L);

        lexAndTestNumberLiteral("-4000000000", LongCrystal.class, -4000000000L);
        lexAndTestNumberLiteral("-22L", LongCrystal.class, -22L);
        lexAndTestNumberLiteral("-47l", LongCrystal.class, -47L);
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_Numbers_BigIntegerLiterals() {
        String bigNumberLiteral = "10000000000000000000";
        lexAndTestNumberLiteral(bigNumberLiteral, BigIntegerCrystal.class, new BigInteger(bigNumberLiteral));
        lexAndTestNumberLiteral("2B", BigIntegerCrystal.class, new BigInteger("2"));
        lexAndTestNumberLiteral("72b", BigIntegerCrystal.class, new BigInteger("72"));

        bigNumberLiteral = "-" + bigNumberLiteral;
        lexAndTestNumberLiteral(bigNumberLiteral, BigIntegerCrystal.class, new BigInteger(bigNumberLiteral));
        lexAndTestNumberLiteral("-2B", BigIntegerCrystal.class, new BigInteger("-2"));
        lexAndTestNumberLiteral("-72b", BigIntegerCrystal.class, new BigInteger("-72"));
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_Numbers_FloatLiterals() {
        lexAndTestNumberLiteral("5.0F", FloatCrystal.class, 5.0F);
        lexAndTestNumberLiteral("6.28f", FloatCrystal.class, 6.28F);

        lexAndTestNumberLiteral("-5.0F", FloatCrystal.class, -5.0F);
        lexAndTestNumberLiteral("-6.28f", FloatCrystal.class, -6.28F);
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_Numbers_DoubleLiterals() {
        lexAndTestNumberLiteral("1.0", DoubleCrystal.class, 1.0);
        lexAndTestNumberLiteral("5.0D", DoubleCrystal.class, 5.0);
        lexAndTestNumberLiteral("3.14d", DoubleCrystal.class, 3.14);

        lexAndTestNumberLiteral("-1.0", DoubleCrystal.class, -1.0);
        lexAndTestNumberLiteral("-5.0D", DoubleCrystal.class, -5.0);
        lexAndTestNumberLiteral("-3.14d", DoubleCrystal.class, -3.14);
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_Numbers_BigDecimalLiterals() {
        // Un-suffixed BigDecimal literals are tested in a later method.
        lexAndTestNumberLiteral("5.0B", BigDecimalCrystal.class, new BigDecimal("5.0"));
        lexAndTestNumberLiteral("3.14b", BigDecimalCrystal.class, new BigDecimal("3.14"));

        lexAndTestNumberLiteral("-5.0B", BigDecimalCrystal.class, new BigDecimal("-5.0"));
        lexAndTestNumberLiteral("-3.14b", BigDecimalCrystal.class, new BigDecimal("-3.14"));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_Numbers_IntegerNumberLiteralOverflow_PositiveValues() {
        String integerNumberStr = "1".repeat(5);
        String longNumberStr = "1".repeat(15);
        String bigIntegerNumberStr = "1".repeat(25);

        // without integer or long suffix
        lexAndTestNumberLiteral(integerNumberStr, IntegerCrystal.class, Integer.valueOf(integerNumberStr));
        lexAndTestNumberLiteral(longNumberStr, LongCrystal.class, Long.valueOf(longNumberStr));
        lexAndTestNumberLiteral(bigIntegerNumberStr, BigIntegerCrystal.class, new BigInteger(bigIntegerNumberStr));

        // with integer suffix
        lexAndTestNumberLiteral(integerNumberStr + "I", IntegerCrystal.class, Integer.valueOf(integerNumberStr));
        lexAndTestNumberLiteral_OverflowError(longNumberStr + "I", IntegerCrystal.class, Integer.MAX_VALUE);
        lexAndTestNumberLiteral_OverflowError(bigIntegerNumberStr + "I", IntegerCrystal.class, Integer.MAX_VALUE);

        // with long suffix
        lexAndTestNumberLiteral(integerNumberStr + "L", LongCrystal.class, Long.valueOf(integerNumberStr));
        lexAndTestNumberLiteral(longNumberStr + "L", LongCrystal.class, Long.valueOf(longNumberStr));
        lexAndTestNumberLiteral_OverflowError(bigIntegerNumberStr + "L", LongCrystal.class, Long.MAX_VALUE);

        // with big integer suffix
        lexAndTestNumberLiteral(integerNumberStr + "B", BigIntegerCrystal.class, new BigInteger(integerNumberStr));
        lexAndTestNumberLiteral(longNumberStr + "B", BigIntegerCrystal.class, new BigInteger(longNumberStr));
        lexAndTestNumberLiteral(bigIntegerNumberStr + "B", BigIntegerCrystal.class, new BigInteger(bigIntegerNumberStr));
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_Numbers_IntegerNumberLiteralOverflow_NegativeValues() {
        String fiveDigits = "1".repeat(5);
        String fifteenDigits = "1".repeat(15);
        String twentyFiveDigits = "1".repeat(25);

        String integerNumberStr = "-" + fiveDigits;
        String longNumberStr = "-" + fifteenDigits;
        String bigIntegerNumberStr = "-" + twentyFiveDigits;

        // without integer or long suffix
        lexAndTestNumberLiteral(integerNumberStr, IntegerCrystal.class, Integer.valueOf(integerNumberStr));
        lexAndTestNumberLiteral(longNumberStr, LongCrystal.class, Long.valueOf(longNumberStr));
        lexAndTestNumberLiteral(bigIntegerNumberStr, BigIntegerCrystal.class, new BigInteger(bigIntegerNumberStr));

        // with integer suffix
        lexAndTestNumberLiteral(integerNumberStr + "I", IntegerCrystal.class, Integer.valueOf(integerNumberStr));
        lexAndTestNumberLiteral_OverflowError(longNumberStr + "I", IntegerCrystal.class, Integer.MIN_VALUE);
        lexAndTestNumberLiteral_OverflowError(bigIntegerNumberStr + "I", IntegerCrystal.class, Integer.MIN_VALUE);

        // with long suffix
        lexAndTestNumberLiteral(integerNumberStr + "L", LongCrystal.class, Long.valueOf(integerNumberStr));
        lexAndTestNumberLiteral(longNumberStr + "L", LongCrystal.class, Long.valueOf(longNumberStr));
        lexAndTestNumberLiteral_OverflowError(bigIntegerNumberStr + "L", LongCrystal.class, Long.MIN_VALUE);

        // with big integer suffix
        lexAndTestNumberLiteral(integerNumberStr + "B", BigIntegerCrystal.class, new BigInteger(integerNumberStr));
        lexAndTestNumberLiteral(longNumberStr + "B", BigIntegerCrystal.class, new BigInteger(longNumberStr));
        lexAndTestNumberLiteral(bigIntegerNumberStr + "B", BigIntegerCrystal.class, new BigInteger(bigIntegerNumberStr));
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_Numbers_FloatingPointNumberLiteralOverflow_PositiveValues() {
        String fiveDigits = "1".repeat(5);
        String fiftyDigits = "1".repeat(50);
        String fiveHundredDigits = "1".repeat(500);

        String floatNumberStr = fiveDigits + "." + fiveDigits;
        String doubleNumberStr = fiftyDigits + "." + fiftyDigits;
        String bigDecimalNumberStr = fiveHundredDigits + "." + fiveHundredDigits;

        // without float or double suffix
        lexAndTestNumberLiteral(floatNumberStr, DoubleCrystal.class, Double.valueOf(floatNumberStr));
        lexAndTestNumberLiteral(doubleNumberStr, DoubleCrystal.class, Double.valueOf(doubleNumberStr));
        lexAndTestNumberLiteral(bigDecimalNumberStr, BigDecimalCrystal.class, new BigDecimal(bigDecimalNumberStr));

        // with float suffix
        lexAndTestNumberLiteral(floatNumberStr + "F", FloatCrystal.class, Float.valueOf(floatNumberStr));
        lexAndTestNumberLiteral_OverflowError(doubleNumberStr + "F", FloatCrystal.class, Float.POSITIVE_INFINITY);
        lexAndTestNumberLiteral_OverflowError(bigDecimalNumberStr + "F", FloatCrystal.class, Float.POSITIVE_INFINITY);

        // with double suffix
        lexAndTestNumberLiteral(floatNumberStr + "D", DoubleCrystal.class, Double.valueOf(floatNumberStr));
        lexAndTestNumberLiteral(doubleNumberStr + "D", DoubleCrystal.class, Double.valueOf(doubleNumberStr));
        lexAndTestNumberLiteral_OverflowError(bigDecimalNumberStr + "D", DoubleCrystal.class, Double.POSITIVE_INFINITY);

        // with big decimal suffix
        lexAndTestNumberLiteral(floatNumberStr + "B", BigDecimalCrystal.class, new BigDecimal(floatNumberStr));
        lexAndTestNumberLiteral(doubleNumberStr + "B", BigDecimalCrystal.class, new BigDecimal(doubleNumberStr));
        lexAndTestNumberLiteral(bigDecimalNumberStr + "B", BigDecimalCrystal.class, new BigDecimal(bigDecimalNumberStr));
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_Numbers_FloatingPointNumberLiteralOverflow_NegativeValues() {
        String fiveDigits = "1".repeat(5);
        String fiftyDigits = "1".repeat(50);
        String fiveHundredDigits = "1".repeat(500);

        String floatNumberStr = "-" + fiveDigits + "." + fiveDigits;
        String doubleNumberStr = "-" + fiftyDigits + "." + fiftyDigits;
        String bigDecimalNumberStr = "-" + fiveHundredDigits + "." + fiveHundredDigits;

        // without float or double suffix
        lexAndTestNumberLiteral(floatNumberStr, DoubleCrystal.class, Double.valueOf(floatNumberStr));
        lexAndTestNumberLiteral(doubleNumberStr, DoubleCrystal.class, Double.valueOf(doubleNumberStr));
        lexAndTestNumberLiteral(bigDecimalNumberStr, BigDecimalCrystal.class, new BigDecimal(bigDecimalNumberStr));

        // with float suffix
        lexAndTestNumberLiteral(floatNumberStr + "F", FloatCrystal.class, Float.valueOf(floatNumberStr));
        lexAndTestNumberLiteral_OverflowError(doubleNumberStr + "F", FloatCrystal.class, Float.NEGATIVE_INFINITY);
        lexAndTestNumberLiteral_OverflowError(bigDecimalNumberStr + "F", FloatCrystal.class, Float.NEGATIVE_INFINITY);

        // with double suffix
        lexAndTestNumberLiteral(floatNumberStr + "D", DoubleCrystal.class, Double.valueOf(floatNumberStr));
        lexAndTestNumberLiteral(doubleNumberStr + "D", DoubleCrystal.class, Double.valueOf(doubleNumberStr));
        lexAndTestNumberLiteral_OverflowError(bigDecimalNumberStr + "D", DoubleCrystal.class, Double.NEGATIVE_INFINITY);

        // with big decimal suffix
        lexAndTestNumberLiteral(floatNumberStr + "B", BigDecimalCrystal.class, new BigDecimal(floatNumberStr));
        lexAndTestNumberLiteral(doubleNumberStr + "B", BigDecimalCrystal.class, new BigDecimal(doubleNumberStr));
        lexAndTestNumberLiteral(bigDecimalNumberStr + "B", BigDecimalCrystal.class, new BigDecimal(bigDecimalNumberStr));
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_Numbers_BigDecimal_WithScaleGreaterThan32() {
        String oneHundredDigits = "1234567890".repeat(10);
        String numberString = oneHundredDigits + "." + oneHundredDigits;
        String bigDecimalLiteral = numberString + "B";
        lexAndTestNumberLiteral(bigDecimalLiteral, BigDecimalCrystal.class, new BigDecimal(numberString));
    }
}
