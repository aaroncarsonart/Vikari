package com.atonementcrystals.dnr.vikari.util;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.Keyword;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.error.Vikari_LexerException;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class UtilsTest {

    @Test
    @Order(1)
    public void testUtils_isIntegerNumber_PositiveCases() {
        String testFailureMessage = "Expected Utils::isIntegerNumber to return true.";

        // positive values
        assertTrue(Utils.isIntegerNumber("2"), testFailureMessage);
        assertTrue(Utils.isIntegerNumber("7i"), testFailureMessage);
        assertTrue(Utils.isIntegerNumber("22I"), testFailureMessage);
        assertTrue(Utils.isIntegerNumber("2147483647"), testFailureMessage); // Integer.MAX_VALUE

        // negative values
        assertTrue(Utils.isIntegerNumber("-2"), testFailureMessage);
        assertTrue(Utils.isIntegerNumber("-7i"), testFailureMessage);
        assertTrue(Utils.isIntegerNumber("-22I"), testFailureMessage);
        assertTrue(Utils.isIntegerNumber("-2147483648"), testFailureMessage); // Integer.MIN_VALUE
    }

    @Test
    @Order(2)
    public void testUtils_isIntegerNumber_NegativeCases() {
        String testFailureMessage = "Expected Utils::isIntegerNumber to return false.";

        // positive values
        assertFalse(Utils.isIntegerNumber("2.0"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("2147483648"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertFalse(Utils.isIntegerNumber("9223372036854775807"), testFailureMessage); // Long.MAX_VALUE
        assertFalse(Utils.isIntegerNumber("9223372036854775808"), testFailureMessage); // Long.MAX_VALUE + 1

        assertFalse(Utils.isIntegerNumber("7.0i"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22.0I"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("7L"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22L"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("7.0l"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22.0L"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("7f"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22F"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("7.0f"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22.0F"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("7d"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22D"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("7.0d"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22.0D"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("7b"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22B"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("7.0b"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("22.0B"), testFailureMessage);

        // negative values
        assertFalse(Utils.isIntegerNumber("-2.0"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-2147483649"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertFalse(Utils.isIntegerNumber("-9223372036854775808"), testFailureMessage); // Long.MIN_VALUE
        assertFalse(Utils.isIntegerNumber("-9223372036854775809"), testFailureMessage); // Long.MIN_VALUE - 1

        assertFalse(Utils.isIntegerNumber("-7.0i"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22.0I"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("-7L"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22L"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-7.0l"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22.0L"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("-7f"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22F"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-7.0f"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22.0F"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("-7d"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22D"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-7.0d"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22.0D"), testFailureMessage);

        assertFalse(Utils.isIntegerNumber("-7b"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22B"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-7.0b"), testFailureMessage);
        assertFalse(Utils.isIntegerNumber("-22.0B"), testFailureMessage);
    }

    @Test
    @Order(3)
    public void testUtils_isLongNumber_PositiveCases() {
        String testFailureMessage = "Expected Utils::isLongNumber to return true.";

        // positive values
        assertTrue(Utils.isLongNumber("2"), testFailureMessage);
        assertTrue(Utils.isLongNumber("7l"), testFailureMessage);
        assertTrue(Utils.isLongNumber("22L"), testFailureMessage);
        assertTrue(Utils.isLongNumber("2147483647"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isLongNumber("2147483648"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isLongNumber("9223372036854775807"), testFailureMessage); // Long.MAX_VALUE

        // negative values
        assertTrue(Utils.isLongNumber("-2"), testFailureMessage);
        assertTrue(Utils.isLongNumber("-7l"), testFailureMessage);
        assertTrue(Utils.isLongNumber("-22L"), testFailureMessage);
        assertTrue(Utils.isLongNumber("-2147483648"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isLongNumber("-2147483649"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isLongNumber("-9223372036854775808"), testFailureMessage); // Long.MIN_VALUE
    }

    @Test
    @Order(4)
    public void testUtils_isLongNumber_NegativeCases() {
        String testFailureMessage = "Expected Utils::isLongNumber to return false.";

        // positive values
        assertFalse(Utils.isLongNumber("2.0"), testFailureMessage);
        assertFalse(Utils.isLongNumber("9223372036854775808"), testFailureMessage); // Long.MAX_VALUE + 1

        assertFalse(Utils.isLongNumber("7i"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22I"), testFailureMessage);
        assertFalse(Utils.isLongNumber("7.0i"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22.0I"), testFailureMessage);

        assertFalse(Utils.isLongNumber("7.0l"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22.0L"), testFailureMessage);

        assertFalse(Utils.isLongNumber("7f"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22F"), testFailureMessage);
        assertFalse(Utils.isLongNumber("7.0f"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22.0F"), testFailureMessage);

        assertFalse(Utils.isLongNumber("7d"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22D"), testFailureMessage);
        assertFalse(Utils.isLongNumber("7.0d"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22.0D"), testFailureMessage);

        assertFalse(Utils.isLongNumber("7b"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22B"), testFailureMessage);
        assertFalse(Utils.isLongNumber("7.0b"), testFailureMessage);
        assertFalse(Utils.isLongNumber("22.0B"), testFailureMessage);

        // negative values
        assertFalse(Utils.isLongNumber("-2.0"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-9223372036854775809"), testFailureMessage); // Long.MIN_VALUE - 1

        assertFalse(Utils.isLongNumber("-7i"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22I"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-7.0i"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22.0I"), testFailureMessage);

        assertFalse(Utils.isLongNumber("-7.0l"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22.0L"), testFailureMessage);

        assertFalse(Utils.isLongNumber("-7f"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22F"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-7.0f"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22.0F"), testFailureMessage);

        assertFalse(Utils.isLongNumber("-7d"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22D"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-7.0d"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22.0D"), testFailureMessage);

        assertFalse(Utils.isLongNumber("-7b"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22B"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-7.0b"), testFailureMessage);
        assertFalse(Utils.isLongNumber("-22.0B"), testFailureMessage);
    }

    @Test
    @Order(5)
    public void testUtils_isBigIntegerNumber_PositiveCases() {
        String testFailureMessage = "Expected Utils::isBigIntegerNumber to return true.";

        // positive values
        assertTrue(Utils.isBigIntegerNumber("2"), testFailureMessage);
        assertTrue(Utils.isBigIntegerNumber("7b"), testFailureMessage);
        assertTrue(Utils.isBigIntegerNumber("22B"), testFailureMessage);
        assertTrue(Utils.isBigIntegerNumber("2147483647"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isBigIntegerNumber("2147483648"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isBigIntegerNumber("9223372036854775807"), testFailureMessage); // Long.MAX_VALUE
        assertTrue(Utils.isBigIntegerNumber("9223372036854775808"), testFailureMessage); // Long.MAX_VALUE + 1

        // negative values
        assertTrue(Utils.isBigIntegerNumber("-2"), testFailureMessage);
        assertTrue(Utils.isBigIntegerNumber("-7b"), testFailureMessage);
        assertTrue(Utils.isBigIntegerNumber("-22B"), testFailureMessage);
        assertTrue(Utils.isBigIntegerNumber("-2147483648"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isBigIntegerNumber("-2147483649"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isBigIntegerNumber("-9223372036854775808"), testFailureMessage); // Long.MIN_VALUE
        assertTrue(Utils.isBigIntegerNumber("-9223372036854775809"), testFailureMessage); // Long.MIN_VALUE - 1
    }

    @Test
    @Order(6)
    public void testUtils_isBigIntegerNumber_NegativeCases() {
        String testFailureMessage = "Expected Utils::isBigIntegerNumber to return false.";

        // positive values
        assertFalse(Utils.isBigIntegerNumber("2.0"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("7.0i"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22.0I"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("7L"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22L"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("7.0l"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22.0L"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("7f"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22F"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("7.0f"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22.0F"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("7d"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22D"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("7.0d"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22.0D"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("7.0b"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("22.0B"), testFailureMessage);

        // negative values
        assertFalse(Utils.isBigIntegerNumber("-2.0"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-7.0i"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22.0I"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("-7L"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22L"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-7.0l"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22.0L"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("-7f"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22F"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-7.0f"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22.0F"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("-7d"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22D"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-7.0d"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22.0D"), testFailureMessage);

        assertFalse(Utils.isBigIntegerNumber("-7.0b"), testFailureMessage);
        assertFalse(Utils.isBigIntegerNumber("-22.0B"), testFailureMessage);
    }

    @Test
    @Order(7)
    public void testUtils_isFloatNumber_PositiveCases() {
        String testFailureMessage = "Expected Utils::isFloatNumber to return true.";

        // positive values
        assertTrue(Utils.isFloatNumber("2"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("7f"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("22F"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("2147483647"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isFloatNumber("2147483648"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isFloatNumber("9223372036854775807"), testFailureMessage); // Long.MAX_VALUE
        assertTrue(Utils.isFloatNumber("9223372036854775808"), testFailureMessage); // Long.MAX_VALUE + 1

        assertTrue(Utils.isFloatNumber("2.0"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("7.0f"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("22.0F"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("2147483647.0"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isFloatNumber("2147483648.0"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isFloatNumber("9223372036854775807.0"), testFailureMessage); // Long.MAX_VALUE
        assertTrue(Utils.isFloatNumber("9223372036854775808.0"), testFailureMessage); // Long.MAX_VALUE + 1

        assertTrue(Utils.isFloatNumber("3.1415926"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("3.1415926f"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("3.1415926F"), testFailureMessage);

        // negative values
        assertTrue(Utils.isFloatNumber("-2"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-7f"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-22F"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-2147483648"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isFloatNumber("-2147483649"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isFloatNumber("-9223372036854775808"), testFailureMessage); // Long.MIN_VALUE
        assertTrue(Utils.isFloatNumber("-9223372036854775809"), testFailureMessage); // Long.MIN_VALUE - 1

        assertTrue(Utils.isFloatNumber("-2.0"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-7.0f"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-22.0F"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-2147483648.0"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isFloatNumber("-2147483649.0"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isFloatNumber("-9223372036854775808.0"), testFailureMessage); // Long.MIN_VALUE
        assertTrue(Utils.isFloatNumber("-9223372036854775809.0"), testFailureMessage); // Long.MIN_VALUE - 1

        assertTrue(Utils.isFloatNumber("-3.1415926"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-3.1415926f"), testFailureMessage);
        assertTrue(Utils.isFloatNumber("-3.1415926F"), testFailureMessage);
    }

    @Test
    @Order(8)
    public void testUtils_isFloatNumber_NegativeCases() {
        String testFailureMessage = "Expected Utils::isFloatNumber to return false.";

        // positive values
        assertFalse(Utils.isFloatNumber("7i"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22I"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("7.0i"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22.0I"), testFailureMessage);

        assertFalse(Utils.isFloatNumber("7L"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22L"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("7.0l"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22.0L"), testFailureMessage);

        assertFalse(Utils.isFloatNumber("7d"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22D"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("7.0d"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22.0D"), testFailureMessage);

        assertFalse(Utils.isFloatNumber("7b"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22B"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("7.0b"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("22.0B"), testFailureMessage);

        // negative values
        assertFalse(Utils.isFloatNumber("-7i"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22I"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-7.0i"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22.0I"), testFailureMessage);

        assertFalse(Utils.isFloatNumber("-7L"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22L"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-7.0l"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22.0L"), testFailureMessage);

        assertFalse(Utils.isFloatNumber("-7d"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22D"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-7.0d"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22.0D"), testFailureMessage);

        assertFalse(Utils.isFloatNumber("-7b"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22B"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-7.0b"), testFailureMessage);
        assertFalse(Utils.isFloatNumber("-22.0B"), testFailureMessage);
    }

    @Test
    @Order(9)
    public void testUtils_isDoubleNumber_PositiveCases() {
        String testFailureMessage = "Expected Utils::isDoubleNumber to return true.";

        // positive values
        assertTrue(Utils.isDoubleNumber("2"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("7d"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("22D"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("2147483647"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isDoubleNumber("2147483648"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isDoubleNumber("9223372036854775807"), testFailureMessage); // Long.MAX_VALUE
        assertTrue(Utils.isDoubleNumber("9223372036854775808"), testFailureMessage); // Long.MAX_VALUE + 1

        assertTrue(Utils.isDoubleNumber("2.0"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("7.0d"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("22.0D"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("2147483647.0"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isDoubleNumber("2147483648.0"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isDoubleNumber("9223372036854775807.0"), testFailureMessage); // Long.MAX_VALUE
        assertTrue(Utils.isDoubleNumber("9223372036854775808.0"), testFailureMessage); // Long.MAX_VALUE + 1

        assertTrue(Utils.isDoubleNumber("3.1415926"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("3.1415926d"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("3.1415926D"), testFailureMessage);

        // negative values
        assertTrue(Utils.isDoubleNumber("-2"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-7d"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-22D"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-2147483648"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isDoubleNumber("-2147483649"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isDoubleNumber("-9223372036854775808"), testFailureMessage); // Long.MIN_VALUE
        assertTrue(Utils.isDoubleNumber("-9223372036854775809"), testFailureMessage); // Long.MIN_VALUE - 1

        assertTrue(Utils.isDoubleNumber("-2.0"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-7.0d"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-22.0D"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-2147483648.0"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isDoubleNumber("-2147483649.0"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isDoubleNumber("-9223372036854775808.0"), testFailureMessage); // Long.MIN_VALUE
        assertTrue(Utils.isDoubleNumber("-9223372036854775809.0"), testFailureMessage); // Long.MIN_VALUE - 1

        assertTrue(Utils.isDoubleNumber("-3.1415926"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-3.1415926d"), testFailureMessage);
        assertTrue(Utils.isDoubleNumber("-3.1415926D"), testFailureMessage);
    }

    @Test
    @Order(10)
    public void testUtils_isDoubleNumber_NegativeCases() {
        String testFailureMessage = "Expected Utils::isDoubleNumber to return false.";

        // positive values
        assertFalse(Utils.isDoubleNumber("7i"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22I"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("7.0i"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22.0I"), testFailureMessage);

        assertFalse(Utils.isDoubleNumber("7L"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22L"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("7.0l"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22.0L"), testFailureMessage);

        assertFalse(Utils.isDoubleNumber("7f"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22F"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("7.0f"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22.0F"), testFailureMessage);

        assertFalse(Utils.isDoubleNumber("7b"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22B"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("7.0b"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("22.0B"), testFailureMessage);

        // negative values
        assertFalse(Utils.isDoubleNumber("-7i"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22I"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-7.0i"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22.0I"), testFailureMessage);

        assertFalse(Utils.isDoubleNumber("-7L"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22L"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-7.0l"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22.0L"), testFailureMessage);

        assertFalse(Utils.isDoubleNumber("-7f"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22F"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-7.0f"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22.0F"), testFailureMessage);

        assertFalse(Utils.isDoubleNumber("-7b"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22B"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-7.0b"), testFailureMessage);
        assertFalse(Utils.isDoubleNumber("-22.0B"), testFailureMessage);
    }

    @Test
    @Order(11)
    public void testUtils_isBigDecimalNumber_PositiveCases() {
        String testFailureMessage = "Expected Utils::isBigDecimalNumber to return true.";

        // positive values
        assertTrue(Utils.isBigDecimalNumber("2"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("7b"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("22B"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("2147483647"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isBigDecimalNumber("2147483648"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isBigDecimalNumber("9223372036854775807"), testFailureMessage); // Long.MAX_VALUE
        assertTrue(Utils.isBigDecimalNumber("9223372036854775808"), testFailureMessage); // Long.MAX_VALUE + 1

        assertTrue(Utils.isBigDecimalNumber("2.0"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("7.0b"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("22.0B"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("2147483647.0"), testFailureMessage); // Integer.MAX_VALUE
        assertTrue(Utils.isBigDecimalNumber("2147483648.0"), testFailureMessage); // Integer.MAX_VALUE + 1
        assertTrue(Utils.isBigDecimalNumber("9223372036854775807.0"), testFailureMessage); // Long.MAX_VALUE
        assertTrue(Utils.isBigDecimalNumber("9223372036854775808.0"), testFailureMessage); // Long.MAX_VALUE + 1

        assertTrue(Utils.isBigDecimalNumber("3.1415926"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("3.1415926b"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("3.1415926B"), testFailureMessage);

        // negative values
        assertTrue(Utils.isBigDecimalNumber("-2"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-7b"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-22B"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-2147483648"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isBigDecimalNumber("-2147483649"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isBigDecimalNumber("-9223372036854775808"), testFailureMessage); // Long.MIN_VALUE
        assertTrue(Utils.isBigDecimalNumber("-9223372036854775809"), testFailureMessage); // Long.MIN_VALUE - 1

        assertTrue(Utils.isBigDecimalNumber("-2.0"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-7.0b"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-22.0B"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-2147483648.0"), testFailureMessage); // Integer.MIN_VALUE
        assertTrue(Utils.isBigDecimalNumber("-2147483649.0"), testFailureMessage); // Integer.MIN_VALUE - 1
        assertTrue(Utils.isBigDecimalNumber("-9223372036854775808.0"), testFailureMessage); // Long.MIN_VALUE
        assertTrue(Utils.isBigDecimalNumber("-9223372036854775809.0"), testFailureMessage); // Long.MIN_VALUE - 1

        assertTrue(Utils.isBigDecimalNumber("-3.1415926"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-3.1415926b"), testFailureMessage);
        assertTrue(Utils.isBigDecimalNumber("-3.1415926B"), testFailureMessage);
    }

    @Test
    @Order(12)
    public void testUtils_isBigDecimalNumber_NegativeCases() {
        String testFailureMessage = "Expected Utils::isBigDecimalNumber to return false.";

        // positive values
        assertFalse(Utils.isBigDecimalNumber("7i"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22I"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("7.0i"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22.0I"), testFailureMessage);

        assertFalse(Utils.isBigDecimalNumber("7L"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22L"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("7.0l"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22.0L"), testFailureMessage);

        assertFalse(Utils.isBigDecimalNumber("7f"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22F"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("7.0f"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22.0F"), testFailureMessage);

        assertFalse(Utils.isBigDecimalNumber("7d"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22D"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("7.0d"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("22.0D"), testFailureMessage);

        // negative values
        assertFalse(Utils.isBigDecimalNumber("-7i"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22I"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-7.0i"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22.0I"), testFailureMessage);

        assertFalse(Utils.isBigDecimalNumber("-7L"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22L"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-7.0l"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22.0L"), testFailureMessage);

        assertFalse(Utils.isBigDecimalNumber("-7f"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22F"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-7.0f"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22.0F"), testFailureMessage);

        assertFalse(Utils.isBigDecimalNumber("-7d"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22D"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-7.0d"), testFailureMessage);
        assertFalse(Utils.isBigDecimalNumber("-22.0D"), testFailureMessage);
    }

    @Test
    @Order(13)
    public void testUtils_stripEnclosure() {
        String enclosure = "`foo`";
        String inner = Utils.stripEnclosure(enclosure, "`", "`");
        assertEquals("foo", inner, "String should have backticks removed by Utils::stripEnclosure.");

        enclosure = "``a << 2 - [7 * a]``";
        inner = Utils.stripEnclosure(enclosure, "``", "``");
        assertEquals("a << 2 - [7 * a]", inner, "String should have capture quotations removed by" +
                "Utils::stripEnclosure.");

        enclosure = "~:Comment enclosure.:~";
        inner = Utils.stripEnclosure(enclosure, "~:", ":~");
        assertEquals("Comment enclosure.", inner, "A comment  should have its prefix and suffix crystals removed by " +
                "Utils::stripEnclosure.");

        enclosure = "This is a string literal not properly quoted on the left side.``";
        try {
            Utils.stripEnclosure(enclosure, "``", "``");
            fail("Utils::stripEnclosure should fail for an improperly enclosed string.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("String missing start enclosure"));
        }

        enclosure = "``This is a string literal not properly quoted on the right side.";
        try {
            Utils.stripEnclosure(enclosure, "``", "``");
            fail("Utils::stripEnclosure should fail for an improperly enclosed string.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("String missing end enclosure"));
        }
    }

    @Test
    @Order(14)
    public void testUtils_isWhitespace() {
        // positive tests
        assertTrue(Utils.isWhitespace(" "), "Single-space string should return true for" +
                "Utils::isWhitespace.");
        assertTrue(Utils.isWhitespace("\t"), "Single-tab string should return true for " +
                "Utils::isWhitespace.");
        assertTrue(Utils.isWhitespace(" \t\t"), "String of a mix of only tabs and spaces should return true for" +
                "Utils::isWhitespace.");
        assertTrue(Utils.isWhitespace("\t  \t"), "String of a mix of only tabs and spaces should return true for" +
                "Utils::isWhitespace.");
        assertTrue(Utils.isWhitespace(" \t\t  \t "), "String of a mix of only tabs and spaces should return true for" +
                "Utils::isWhitespace.");

        // negative tests
        assertFalse(Utils.isWhitespace("a"), "String with non-whitespace characters should return false for " +
                "Utils::isWhitespace.");
        assertFalse(Utils.isWhitespace(" a\t\t"), "String with non-whitespace characters should return false for " +
                "Utils::isWhitespace.");
    }

    @Test
    @Order(15)
    public void testUtils_isBacktickQuotedIdentifier() {
        // positive tests
        assertTrue(Utils.isBacktickQuotedIdentifier("`:`"), "A character literal is a quoted identifier.");
        assertTrue(Utils.isBacktickQuotedIdentifier("`foo`"), "A string enclosed in single quotations is a quoted " +
                "identifier.");

        // negative tests
        assertFalse(Utils.isBacktickQuotedIdentifier("``"), "Empty single quotations are not a quoted identifier.");
        assertFalse(Utils.isBacktickQuotedIdentifier("missing left backtick quote`"), "A string missing a left " +
                "backtick quote is not a quoted identifier.");
        assertFalse(Utils.isBacktickQuotedIdentifier("`missing right backtick quote"), "A string missing a right " +
                "backtick quote is not a quoted identifier.");
    }

    @Test
    @Order(16)
    public void testUtils_isSword() {
        // positive tests
        String sword = "_";
        assertTrue(Utils.isSword(sword), "Strings containing only underscores should return true for Utils::isSword.");
        sword = "__";
        assertTrue(Utils.isSword(sword), "Strings containing only underscores should return true for Utils::isSword.");
        sword = "___";
        assertTrue(Utils.isSword(sword), "Strings containing only underscores should return true for Utils::isSword.");
        sword = "____";
        assertTrue(Utils.isSword(sword), "Strings containing only underscores should return true for Utils::isSword.");

        // negative tests
        String notSword = "foo";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
        notSword = "_foo";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
        notSword = "foo_bar";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
        notSword = "___foo_bar___";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
        notSword = " _";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
        notSword = "_ ";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
        notSword = " _ ";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
        notSword = "_ _";
        assertFalse(Utils.isSword(notSword), "Strings containing non-underscore characters should return false for " +
                "Utils::isSword.");
    }

    @Test
    @Order(17)
    public void testUtils_isBooleanLiteral() {
        // positive tests
        String booleanLiteral = Keyword.TRUE.getIdentifier();
        assertTrue(Utils.isBooleanLiteral(booleanLiteral), "Strings matching a boolean literal keyword should return " +
                "true for Utils::isBooleanLiteral.");
        booleanLiteral = Keyword.FALSE.getIdentifier();
        assertTrue(Utils.isBooleanLiteral(booleanLiteral), "Strings matching a boolean literal keyword should return " +
                "true for Utils::isBooleanLiteral.");

        // negative tests
        String notBooleanLiteral = "trueFoo";
        assertFalse(Utils.isBooleanLiteral(notBooleanLiteral), "Strings not matching a boolean literal keyword " +
                "should return false for Utils::isBooleanLiteral.");
        notBooleanLiteral = "footrue";
        assertFalse(Utils.isBooleanLiteral(notBooleanLiteral), "Strings not matching a boolean literal keyword " +
                "should return false for Utils::isBooleanLiteral.");
        notBooleanLiteral = "_true_";
        assertFalse(Utils.isBooleanLiteral(notBooleanLiteral), "Strings not matching a boolean literal keyword " +
                "should return false for Utils::isBooleanLiteral.");
        notBooleanLiteral = "foo_true";
        assertFalse(Utils.isBooleanLiteral(notBooleanLiteral), "Strings not matching a boolean literal keyword " +
                "should return false for Utils::isBooleanLiteral.");
        notBooleanLiteral = "___";
        assertFalse(Utils.isBooleanLiteral(notBooleanLiteral), "Strings not matching a boolean literal keyword " +
                "should return false for Utils::isBooleanLiteral.");
        notBooleanLiteral = " ";
        assertFalse(Utils.isBooleanLiteral(notBooleanLiteral), "Strings not matching a boolean literal keyword " +
                "should return false for Utils::isBooleanLiteral.");
    }

    @Test
    @Order(18)
    public void testUtils_isEnclosedString() {
        // positive tests
        String enclosedString = "``foo``";
        String leftEnclosure = "``";
        String rightEnclosure = "``";
        assertTrue(Utils.isEnclosedString(enclosedString, leftEnclosure, rightEnclosure), "An enclosed string should " +
                "return true for Utils::isEnclosedString.");

        enclosedString = "`bar`";
        leftEnclosure = "`";
        rightEnclosure = "`";
        assertTrue(Utils.isEnclosedString(enclosedString, leftEnclosure, rightEnclosure), "An enclosed string should " +
                "return true for Utils::isEnclosedString.");

        enclosedString = "~:comment:~";
        leftEnclosure = "~:";
        rightEnclosure = ":~";
        assertTrue(Utils.isEnclosedString(enclosedString, leftEnclosure, rightEnclosure), "An enclosed string should " +
                "return true for Utils::isEnclosedString.");

        enclosedString = "~::~";
        leftEnclosure = "~:";
        rightEnclosure = ":~";
        assertTrue(Utils.isEnclosedString(enclosedString, leftEnclosure, rightEnclosure), "An empty enclosed string " +
                "with different enclosures should return true for Utils::isEnclosedString.");

        enclosedString = "````";
        leftEnclosure = "``";
        rightEnclosure = "``";
        assertTrue(Utils.isEnclosedString(enclosedString, leftEnclosure, rightEnclosure), "An empty enclosed string " +
                "with identical enclosures should return true for Utils::isEnclosedString.");

        // negative tests
        String nonEnclosedString = "comment:~";
        leftEnclosure = "~:";
        rightEnclosure = ":~";
        assertFalse(Utils.isEnclosedString(nonEnclosedString, leftEnclosure, rightEnclosure), "A string missing the " +
                "left enclosure should return false for Utils::isEnclosedString.");

        nonEnclosedString = "~:comment";
        leftEnclosure = "~:";
        rightEnclosure = ":~";
        assertFalse(Utils.isEnclosedString(nonEnclosedString, leftEnclosure, rightEnclosure), "A string missing the " +
                "right enclosure should return false for Utils::isEnclosedString.");

        nonEnclosedString = "``";
        leftEnclosure = "``";
        rightEnclosure = "``";
        assertFalse(Utils.isEnclosedString(nonEnclosedString, leftEnclosure, rightEnclosure), "A string matching " +
                "identical enclosures should return false for Utils::isEnclosedString.");
    }

    @Test
    @Order(19)
    public void testUtils_isStringLiteral() {
        // positive tests
        String stringLiteral = "``foo``";
        assertTrue(Utils.isStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isStringLiteral.");
        stringLiteral = "``int:Integer << 5``";
        assertTrue(Utils.isStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isStringLiteral.");
        stringLiteral = "``This token contains \\`` a quoted capture quotation.``";
        assertTrue(Utils.isStringLiteral(stringLiteral), "A string that contains a quoted capture quotation " +
                " that is not at the end of the string should return true for Utils::isStringLiteral.");
        stringLiteral = "``This token contains a backslash. \\ ``";
        assertTrue(Utils.isStringLiteral(stringLiteral), "A string that contains a backslash not immediately " +
                " adjacent to the closing capture quotation should return true for Utils::isStringLiteral.");

        // negative tests
        String nonStringLiteral = "``foo";
        assertFalse(Utils.isStringLiteral(nonStringLiteral), "A non-string literal should return false for " +
                "Utils::isStringLiteral.");
        nonStringLiteral = "foo``";
        assertFalse(Utils.isStringLiteral(nonStringLiteral), "A non-string literal should return false for " +
                "Utils::isStringLiteral.");
        nonStringLiteral = "foo";
        assertFalse(Utils.isStringLiteral(nonStringLiteral), "A non-string literal should return false for " +
                "Utils::isStringLiteral.");
        nonStringLiteral = "int:Integer << 5";
        assertFalse(Utils.isStringLiteral(nonStringLiteral), "A non-string literal should return false for " +
                "Utils::isStringLiteral.");
        nonStringLiteral = "str:String << ``I am a string.``, :str";
        assertFalse(Utils.isStringLiteral(nonStringLiteral), "A string that contains a string literal but isn't " +
                "entirely a string literal should return false for Utils::isStringLiteral.");
        nonStringLiteral = "``This token contains a quoted capture quotation: \\``";
        assertFalse(Utils.isStringLiteral(nonStringLiteral), "A string that is ended by a quoted capture quotation " +
                "should return false for Utils::isStringLiteral.");
        nonStringLiteral = "This token contains a quoted capture quotation: \\``";
        assertFalse(Utils.isStringLiteral(nonStringLiteral), "A string that does not contain an opening capture " +
                "quotation and is ended by a quoted capture quotation should return false for Utils::isStringLiteral.");
    }

    @Test
    @Order(20)
    public void testUtils_isStartOfStringLiteral() {
        // positive tests
        String stringLiteral = "``foo``";
        assertTrue(Utils.isStartOfStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isStartOfStringLiteral.");
        stringLiteral = "``int:Integer << 5``";
        assertTrue(Utils.isStartOfStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isStartOfStringLiteral.");
        String nonStringLiteral = "``foo";
        assertTrue(Utils.isStartOfStringLiteral(nonStringLiteral), "The start of a multi-line string literal should " +
                "return true for Utils::isStartOfStringLiteral.");

        // negative tests
        nonStringLiteral = "foo``";
        assertFalse(Utils.isStartOfStringLiteral(nonStringLiteral), "The end of a multi-string literal should return " +
                "false for Utils::isStartOfStringLiteral.");
        nonStringLiteral = "foo";
        assertFalse(Utils.isStartOfStringLiteral(nonStringLiteral), "A reference should return false for " +
                "Utils::isStartOfStringLiteral.");
        nonStringLiteral = "int:Integer << 5";
        assertFalse(Utils.isStartOfStringLiteral(nonStringLiteral), "A Vikari code statement should return false for " +
                "Utils::isStartOfStringLiteral.");
        nonStringLiteral = "str:String << ``I am a string.``, :str";
        assertFalse(Utils.isStartOfStringLiteral(nonStringLiteral), "A string that contains a string literal but " +
                "isn't entirely a string literal should return false for Utils::isStartOfStringLiteral.");
    }

    @Test
    @Order(21)
    public void testUtils_isEndOfStringLiteral() {
        // positive tests
        String stringLiteral = "``foo``";
        assertTrue(Utils.isEndOfStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isEndOfStringLiteral.");
        stringLiteral = "``int:Integer << 5``";
        assertTrue(Utils.isEndOfStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isEndOfStringLiteral.");
        String nonStringLiteral = "foo``";
        assertTrue(Utils.isEndOfStringLiteral(nonStringLiteral), "The end of a multi-string literal should return " +
                "true for Utils::isEndOfStringLiteral.");
        stringLiteral = "``This token contains \\`` a quoted capture quotation.``";
        assertTrue(Utils.isEndOfStringLiteral(stringLiteral), "A string that contains a quoted capture quotation " +
                " that is not at the end of the string should return true for Utils::isEndOfStringLiteral.");
        stringLiteral = "This token contains \\`` a quoted capture quotation.``";
        assertTrue(Utils.isEndOfStringLiteral(stringLiteral), "A string that contains a quoted capture quotation " +
                " that is not at the end of the string should return true for Utils::isEndOfStringLiteral.");
        stringLiteral = "``This token contains a backslash. \\ ``";
        assertTrue(Utils.isEndOfStringLiteral(stringLiteral), "A string that contains a backslash not immediately " +
                " adjacent to the closing capture quotation should return true for Utils::isEndOfStringLiteral.");
        stringLiteral = "This token contains a backslash. \\ ``";
        assertTrue(Utils.isEndOfStringLiteral(stringLiteral), "A string that contains a backslash not immediately " +
                " adjacent to the closing capture quotation should return true for Utils::isEndOfStringLiteral.");

        // negative tests
        nonStringLiteral = "``foo";
        assertFalse(Utils.isEndOfStringLiteral(nonStringLiteral), "The start of a multi-line string literal should " +
                "return false for Utils::isEndOfStringLiteral.");
        nonStringLiteral = "foo";
        assertFalse(Utils.isEndOfStringLiteral(nonStringLiteral), "A reference should return false for " +
                "Utils::isEndOfStringLiteral.");
        nonStringLiteral = "int:Integer << 5";
        assertFalse(Utils.isEndOfStringLiteral(nonStringLiteral), "A Vikari code statement should return false for " +
                "Utils::isEndOfStringLiteral.");
        nonStringLiteral = "str:String << ``I am a string.``, :str";
        assertFalse(Utils.isEndOfStringLiteral(nonStringLiteral), "A string that contains a string literal but " +
                "isn't entirely a string literal should return false for Utils::isEndOfStringLiteral.");
        nonStringLiteral = "``This token contains a quoted capture quotation: \\``";
        assertFalse(Utils.isEndOfStringLiteral(nonStringLiteral), "A string that is ended by a quoted capture quotation " +
                "should return false for Utils::isEndOfStringLiteral.");
        nonStringLiteral = "This token contains a quoted capture quotation: \\``";
        assertFalse(Utils.isEndOfStringLiteral(nonStringLiteral), "A string that is ended by a quoted capture quotation " +
                "should return false for Utils::isEndOfStringLiteral.");
    }

    @Test
    @Order(22)
    public void testUtils_isSingleLineComment() {
        // positive tests
        String comment = "~:This is a comment.:~";
        assertTrue(Utils.isSingleLineComment(comment), "A single-line comment should return true for " +
                "Utils::isSingleLineComment.");
        String emptyComment = "~::~";
        assertTrue(Utils.isSingleLineComment(emptyComment), "An empty comment should return true for " +
                "Utils::isSingleLineComment.");

        // negative tests
        String startOfComment = "~:Start of a multi-line comment.";
        assertFalse(Utils.isSingleLineComment(startOfComment), "The start of a multi-line comment should return" +
                "false for Utils::isSingleLineComment.");
        String endOfComment = "End of a multi-line comment.:~";
        assertFalse(Utils.isSingleLineComment(endOfComment), "The end of a multi-line comment should return " +
                "false for Utils::isSingleLineComment.");
        String notAComment = "int:Integer << 5";
        assertFalse(Utils.isSingleLineComment(notAComment), "A string that is not a comment should return " +
                "false for Utils::isSingleLineComment.");
        String stringContainingAComment = "int:Integer << 5 ~:I am a comment.:~ + foo";
        assertFalse(Utils.isSingleLineComment(stringContainingAComment), "A string that contains a comment but isn't " +
                "entirely a comment should return false for Utils::isSingleLineComment.");
    }

    @Test
    @Order(23)
    public void testUtils_isStartOfComment() {
        // positive tests
        String comment = "~:This is a comment.:~";
        assertTrue(Utils.isStartOfComment(comment), "A single-line comment should return true for " +
                "Utils::isStartOfComment.");
        String emptyComment = "~::~";
        assertTrue(Utils.isStartOfComment(emptyComment), "An empty comment should return true for " +
                "Utils::isStartOfComment.");
        String startOfComment = "~:Start of a multi-line comment.";
        assertTrue(Utils.isStartOfComment(startOfComment), "The start of a multi-line comment should return true for " +
                "Utils::isStartOfComment.");

        // negative tests
        String endOfComment = "End of a multi-line comment.:~";
        assertFalse(Utils.isStartOfComment(endOfComment), "The end of a multi-line comment should return " +
                "false for Utils::isStartOfComment.");
        String notAComment = "int:Integer << 5";
        assertFalse(Utils.isStartOfComment(notAComment), "A string that is not a comment should return " +
                "false for Utils::isStartOfComment.");
        String stringContainingAComment = "int:Integer << 5 ~:I am a comment.:~ + foo";
        assertFalse(Utils.isStartOfComment(stringContainingAComment), "A string that contains a comment but isn't " +
                "entirely a comment should return false for Utils::isStartOfComment.");
    }

    @Test
    @Order(24)
    public void testUtils_isEndOfComment() {
        // positive tests
        String comment = "~:This is a comment.:~";
        assertTrue(Utils.isEndOfComment(comment), "A single-line comment should return true for " +
                "Utils::isEndOfComment.");
        String emptyComment = "~::~";
        assertTrue(Utils.isEndOfComment(emptyComment), "An empty comment should return true for " +
                "Utils::isEndOfComment.");
        String endOfComment = "End of a multi-line comment.:~";
        assertTrue(Utils.isEndOfComment(endOfComment), "The end of a multi-line comment should return false for " +
                "Utils::isEndOfComment.");
        String commentWithQuotedSuffixToken = "~:This comment contains \\:~ a quoted comment suffix token.:~";
        assertTrue(Utils.isEndOfComment(commentWithQuotedSuffixToken), "A string that contains a quoted comment " +
                "suffix token that is not at the end of the string should return true for Utils::isEndOfComment.");
        String stringWithQuotedSuffixToken = "This string contains \\:~ a quoted comment suffix token.:~";
        assertTrue(Utils.isEndOfComment(stringWithQuotedSuffixToken), "A string that contains a quoted comment " +
                "suffix token that is not at the end of the string should return true for Utils::isEndOfComment.");
        String commentWithBackslash = "~:This comment contains a backslash. \\ :~";
        assertTrue(Utils.isEndOfComment(commentWithBackslash), "A string that contains a backslash not immediately " +
                " adjacent to the closing comment suffix token should return true for Utils::isEndOfComment.");
        String stringWithBackslash = "This string contains a backslash. \\ :~";
        assertTrue(Utils.isEndOfComment(stringWithBackslash), "A string that contains a backslash not immediately " +
                " adjacent to the closing comment suffix token should return true for Utils::isEndOfComment.");

        // negative tests
        String startOfComment = "~:Start of a multi-line comment.";
        assertFalse(Utils.isEndOfComment(startOfComment), "The start of a multi-line comment should return true for " +
                "Utils::isEndOfComment.");
        String notAComment = "int:Integer << 5";
        assertFalse(Utils.isEndOfComment(notAComment), "A string that is not a comment should return " +
                "false for Utils::isEndOfComment.");
        String stringContainingAComment = "int:Integer << 5 ~:I am a comment.:~ + foo";
        assertFalse(Utils.isEndOfComment(stringContainingAComment), "A string that contains a comment but isn't " +
                "entirely a comment should return false for Utils::isEndOfComment.");
        String stringEndingInEscapedSuffixToken = "~:This string contains a quoted comment suffix token: \\:~";
        assertFalse(Utils.isEndOfComment(stringEndingInEscapedSuffixToken), "A string that is ended by a quoted " +
                "comment suffix token should return false for Utils::isEndOfComment.");
        stringEndingInEscapedSuffixToken = "This string contains a quoted comment suffix token: \\:~";
        assertFalse(Utils.isEndOfComment(stringEndingInEscapedSuffixToken), "A string that is ended by a quoted " +
                "comment suffix token should return false for Utils::isEndOfComment.");
    }

    @Test
    @Order(25)
    public void testUtils_showInvisibles() {
        // positive tests
        String containingInvisibles = "\t";
        String expected = "→";
        String actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string containing the tabs, newlines, and spaces should have these " +
                "characters replaced by \"·\", \"→\", and \"¶\" respectively when passed to Utils::showInvisibles.");
        containingInvisibles = " ";
        expected = "·";
        actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string containing the tabs, newlines, and spaces should have these " +
                "characters replaced by \"·\", \"→\", and \"¶\" respectively when passed to Utils::showInvisibles.");
        containingInvisibles = "\n";
        expected = "¶";
        actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string containing the tabs, newlines, and spaces should have these " +
                "characters replaced by \"·\", \"→\", and \"¶\" respectively when passed to Utils::showInvisibles.");
        containingInvisibles = "\tInvisible characters\n";
        expected = "→Invisible·characters¶";
        actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string containing the tabs, newlines, and spaces should have these " +
                "characters replaced by \"·\", \"→\", and \"¶\" respectively when passed to Utils::showInvisibles.");
        containingInvisibles = "\t\t\t";
        expected = "→→→";
        actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string containing the tabs, newlines, and spaces should have these " +
                "characters replaced by \"·\", \"→\", and \"¶\" respectively when passed to Utils::showInvisibles.");
        containingInvisibles = "   ";
        expected = "···";
        actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string containing the tabs, newlines, and spaces should have these " +
                "characters replaced by \"·\", \"→\", and \"¶\" respectively when passed to Utils::showInvisibles.");
        containingInvisibles = "\n\n\n";
        expected = "¶¶¶";
        actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string containing the tabs, newlines, and spaces should have these " +
                "characters replaced by \"·\", \"→\", and \"¶\" respectively when passed to Utils::showInvisibles.");

        // negative tests
        containingInvisibles = "foo:Integer<<5";
        expected = "foo:Integer<<5";
        actual = Utils.showInvisibles(containingInvisibles);
        assertEquals(expected, actual, "A string that does not contain tabs, newlines, and spaces should be changed " +
                "when passed to Utils::showInvisibles.");
    }

    @Test
    @Order(26)
    public void testUtils_getSimpleClassName() {
        // positive tests
        AtonementCrystal className = new ReferenceCrystal("referenceIdentifier");
        String expected = "Reference";
        String actual = Utils.getSimpleClassName(className);
        assertEquals(expected, actual, "Class names ending with \"Crystal\" should have this suffix removed when " +
                "passed to Utils::getSimpleClassName.");
        className = new FunctionCallOperatorCrystal();
        expected = "FunctionCallOperator";
        actual = Utils.getSimpleClassName(className);
        assertEquals(expected, actual, "Class names ending with \"Crystal\" should have this suffix removed when " +
                "passed to Utils::getSimpleClassName.");
        className = new AtonementCrystal("crystalIdentifier");
        expected = "AtonementCrystal";
        actual = Utils.getSimpleClassName(className);
        assertEquals(expected, actual, "The AtonementCrystal class should not have its name altered when passed to " +
                "Utils::getSimpleClassName.");
    }

    @Test
    @Order(27)
    public void testUtils_countOccurrences() {
        // Expect no matches.
        String sourceString = "foo";
        String searchString = "bar";
        int regionEnd = sourceString.length();
        int expected = 0;
        int actual = Utils.countOccurrences(sourceString, searchString, regionEnd);
        assertEquals(expected, actual, "Unexpected result for countOccurrences().");

        // Expect one match.
        sourceString = "foo bar";
        searchString = "foo";
        regionEnd = sourceString.length();
        expected = 1;
        actual = Utils.countOccurrences(sourceString, searchString, regionEnd);
        assertEquals(expected, actual, "Unexpected result for countOccurrences().");

        sourceString = "foo bar";
        searchString = "bar";
        regionEnd = sourceString.length();
        expected = 1;
        actual = Utils.countOccurrences(sourceString, searchString, regionEnd);
        assertEquals(expected, actual, "Unexpected result for countOccurrences().");

        // Expect no match because of regionEnd.
        sourceString = "foo bar";
        searchString = "bar";
        regionEnd = "foo".length();
        expected = 0;
        actual = Utils.countOccurrences(sourceString, searchString, regionEnd);
        assertEquals(expected, actual, "Unexpected result for countOccurrences().");

        // Expect multiple matches.
        sourceString = "foo bar foo baz";
        searchString = "foo";
        regionEnd = sourceString.length();
        expected = 2;
        actual = Utils.countOccurrences(sourceString, searchString, regionEnd);
        assertEquals(expected, actual, "Unexpected result for countOccurrences().");
    }

    @Test
    @Order(28)
    public void testHasIntegerSuffix() {
        // positive tests
        String testFailureMessage = "Expected Utils::hasIntegerSuffix to return true.";

        assertTrue(Utils.hasIntegerSuffix("7i"), testFailureMessage);
        assertTrue(Utils.hasIntegerSuffix("22I"), testFailureMessage);

        // negative tests
        testFailureMessage = "Expected Utils::hasIntegerSuffix to return false.";

        assertFalse(Utils.hasIntegerSuffix("2"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("7l"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22L"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("7b"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasIntegerSuffix("7f"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22F"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("7d"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22D"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("7b"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasIntegerSuffix("2.0"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("7.0f"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22.0F"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("7.0d"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22.0D"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("7.0b"), testFailureMessage);
        assertFalse(Utils.hasIntegerSuffix("22.0B"), testFailureMessage);

        assertFalse(Utils.hasIntegerSuffix("3.1415926"), testFailureMessage);
    }

    @Test
    @Order(29)
    public void testHasLongSuffix() {
        // positive tests
        String testFailureMessage = "Expected Utils::hasLongSuffix to return true.";

        assertTrue(Utils.hasLongSuffix("7l"), testFailureMessage);
        assertTrue(Utils.hasLongSuffix("22L"), testFailureMessage);

        // negative tests
        testFailureMessage = "Expected Utils::hasLongSuffix to return false.";

        assertFalse(Utils.hasLongSuffix("2"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("7i"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22I"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("7b"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasLongSuffix("7f"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22F"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("7d"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22D"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("7b"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasLongSuffix("2.0"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("7.0f"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22.0F"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("7.0d"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22.0D"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("7.0b"), testFailureMessage);
        assertFalse(Utils.hasLongSuffix("22.0B"), testFailureMessage);

        assertFalse(Utils.hasLongSuffix("3.1415926"), testFailureMessage);
    }

    @Test
    @Order(30)
    public void testHasFloatSuffix() {
        // positive tests
        String testFailureMessage = "Expected Utils::hasFloatSuffix to return true.";

        assertTrue(Utils.hasFloatSuffix("7f"), testFailureMessage);
        assertTrue(Utils.hasFloatSuffix("22F"), testFailureMessage);
        assertTrue(Utils.hasFloatSuffix("7.0f"), testFailureMessage);
        assertTrue(Utils.hasFloatSuffix("22.0F"), testFailureMessage);
        assertTrue(Utils.hasFloatSuffix("3.1415926f"), testFailureMessage);
        assertTrue(Utils.hasFloatSuffix("3.1415926F"), testFailureMessage);

        // negative tests
        testFailureMessage = "Expected Utils::hasFloatSuffix to return false.";

        assertFalse(Utils.hasFloatSuffix("2"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("7i"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("22I"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("7l"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("22L"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("7b"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasFloatSuffix("7d"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("22D"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("7B"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasFloatSuffix("2.0"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("7.0d"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("22.0D"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("7.0b"), testFailureMessage);
        assertFalse(Utils.hasFloatSuffix("22.0B"), testFailureMessage);

        assertFalse(Utils.hasFloatSuffix("3.1415926"), testFailureMessage);
    }

    @Test
    @Order(31)
    public void testHasDoubleSuffix() {
        // positive tests
        String testFailureMessage = "Expected Utils::hasDoubleSuffix to return true.";

        assertTrue(Utils.hasDoubleSuffix("7d"), testFailureMessage);
        assertTrue(Utils.hasDoubleSuffix("22D"), testFailureMessage);
        assertTrue(Utils.hasDoubleSuffix("7.0d"), testFailureMessage);
        assertTrue(Utils.hasDoubleSuffix("22.0D"), testFailureMessage);
        assertTrue(Utils.hasDoubleSuffix("3.1415926d"), testFailureMessage);
        assertTrue(Utils.hasDoubleSuffix("3.1415926D"), testFailureMessage);

        // negative tests
        testFailureMessage = "Expected Utils::hasDoubleSuffix to return false.";

        assertFalse(Utils.hasDoubleSuffix("2"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("7i"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("22I"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("7l"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("22L"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("7b"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasDoubleSuffix("7f"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("22F"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("7B"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("22B"), testFailureMessage);

        assertFalse(Utils.hasDoubleSuffix("2.0"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("7.0f"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("22.0F"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("7.0b"), testFailureMessage);
        assertFalse(Utils.hasDoubleSuffix("22.0B"), testFailureMessage);

        assertFalse(Utils.hasDoubleSuffix("3.1415926"), testFailureMessage);
    }

    @Test
    @Order(32)
    public void testHasBigSuffix() {
        // positive tests
        String testFailureMessage = "Expected Utils::hasBigSuffix to return true.";

        assertTrue(Utils.hasBigSuffix("7b"), testFailureMessage);
        assertTrue(Utils.hasBigSuffix("22B"), testFailureMessage);
        assertTrue(Utils.hasBigSuffix("7.0b"), testFailureMessage);
        assertTrue(Utils.hasBigSuffix("22.0B"), testFailureMessage);
        assertTrue(Utils.hasBigSuffix("3.1415926b"), testFailureMessage);
        assertTrue(Utils.hasBigSuffix("3.1415926B"), testFailureMessage);

        // negative tests
        testFailureMessage = "Expected Utils::hasBigSuffix to return false.";

        assertFalse(Utils.hasBigSuffix("2"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("7i"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("22I"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("7l"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("22L"), testFailureMessage);

        assertFalse(Utils.hasBigSuffix("7f"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("22F"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("7d"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("2DF"), testFailureMessage);

        assertFalse(Utils.hasBigSuffix("2.0"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("7.0f"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("22.0F"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("7.0d"), testFailureMessage);
        assertFalse(Utils.hasBigSuffix("22.0D"), testFailureMessage);

        assertFalse(Utils.hasBigSuffix("3.1415926"), testFailureMessage);
    }

    @Test
    @Order(33)
    public void testTrimLastCharacter() {
        String testFailureMessage = "Unexpected result from Utils::trimLastCharacter.";
        assertEquals("1", Utils.trimLastCharacter("1i"), testFailureMessage);
        assertEquals("2", Utils.trimLastCharacter("2I"), testFailureMessage);
        assertEquals("3", Utils.trimLastCharacter("3l"), testFailureMessage);
        assertEquals("4", Utils.trimLastCharacter("4L"), testFailureMessage);
        assertEquals("5", Utils.trimLastCharacter("5b"), testFailureMessage);
        assertEquals("6", Utils.trimLastCharacter("6B"), testFailureMessage);
        assertEquals("7.0", Utils.trimLastCharacter("7.0f"), testFailureMessage);
        assertEquals("8.0", Utils.trimLastCharacter("8.0F"), testFailureMessage);
        assertEquals("9.0", Utils.trimLastCharacter("9.0d"), testFailureMessage);
        assertEquals("10.0", Utils.trimLastCharacter("10.0D"), testFailureMessage);
        assertEquals("11.0", Utils.trimLastCharacter("11.0b"), testFailureMessage);
        assertEquals("12.0", Utils.trimLastCharacter("12.0B"), testFailureMessage);
    }

    @Test
    @Order(34)
    public void testIsCrystalIdentifier() {
        assertTrue(Utils.isCrystalIdentifier("a"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("foo"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("foo_bar"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("a2"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("_a2"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("a2_"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("_a2_"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("_a_2_"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("`This is a sentence.`"), "Expected to be a valid crystal identifier.");
        assertTrue(Utils.isCrystalIdentifier("`a:Integer << 3 + 7`"), "Expected to be a valid crystal identifier.");

        assertFalse(Utils.isCrystalIdentifier("3"), "Expected to not be a valid crystal identifier.");
        assertFalse(Utils.isCrystalIdentifier("3a"), "Expected to not be a valid crystal identifier.");
        assertFalse(Utils.isCrystalIdentifier("Foo"), "Expected to not be a valid crystal identifier.");
        assertFalse(Utils.isCrystalIdentifier("`foo"), "Expected to not be a valid crystal identifier.");
        assertFalse(Utils.isCrystalIdentifier("foo`"), "Expected to not be a valid crystal identifier.");
    }

    @Test
    @Order(35)
    public void testIsFieldRegionIdentifier() {
        assertTrue(Utils.isFieldRegionIdentifier("a"), "Expected to be a valid field region identifier.");
        assertTrue(Utils.isFieldRegionIdentifier("foo"), "Expected to be a valid field region identifier.");
        assertTrue(Utils.isFieldRegionIdentifier("foo_bar"), "Expected to be a valid field region identifier.");
        assertTrue(Utils.isFieldRegionIdentifier("a2"), "Expected to be a valid field region identifier.");
        assertTrue(Utils.isFieldRegionIdentifier("_a2"), "Expected to be a valid field region identifier.");
        assertTrue(Utils.isFieldRegionIdentifier("a2_"), "Expected to be a valid field region identifier.");
        assertTrue(Utils.isFieldRegionIdentifier("_a2_"), "Expected to be a valid field region identifier.");
        assertTrue(Utils.isFieldRegionIdentifier("_a_2_"), "Expected to be a valid field region identifier.");

        assertFalse(Utils.isFieldRegionIdentifier("`This is a sentence.`"), "Expected to not be a valid field region " +
                "identifier.");
        assertFalse(Utils.isFieldRegionIdentifier("`a:Integer << 3 + 7`"), "Expected to not be a valid field region " +
                "identifier.");
        assertFalse(Utils.isFieldRegionIdentifier("3"), "Expected to not be a valid field region identifier.");
        assertFalse(Utils.isFieldRegionIdentifier("3a"), "Expected to not be a valid field region identifier.");
        assertFalse(Utils.isFieldRegionIdentifier("Foo"), "Expected to not be a valid field region identifier.");
        assertFalse(Utils.isFieldRegionIdentifier("`foo"), "Expected to not be a valid field region identifier.");
        assertFalse(Utils.isFieldRegionIdentifier("foo`"), "Expected to not be a valid field region identifier.");
    }

    @Test
    @Order(36)
    public void testIsTypeIdentifier() {
        assertTrue(Utils.isTypeIdentifier("Foo"), "Expected to  be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("BarBaz"), "Expected to  be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("A"), "Expected to  be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("Ba"), "Expected to be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("Cfoo"), "Expected to be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("Dfoo_bar"), "Expected to be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("E2"), "Expected to be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("F_a2"), "Expected to be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("Ha2_"), "Expected to be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("H_a2_"), "Expected to be a valid type identifier.");
        assertTrue(Utils.isTypeIdentifier("I_a_2_"), "Expected to be a valid type identifier.");

        assertFalse(Utils.isTypeIdentifier("a"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("foo"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("foo_bar"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("2"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("_a2"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("a2_"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("_a2_"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("_a_2_"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("`This is a sentence.`"), "Expected to not be a valid type identifier.");
        assertFalse(Utils.isTypeIdentifier("`a:Integer << 3 + 7`"), "Expected to not be a valid type identifier.");
    }

    @Test
    @Order(37)
    public void testUtils_isEscapedByBackslash() {
        // negative tests
        assertFalse(Utils.isEscapedByBackslash("", 0), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("a", 0), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("aa", 1), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("aaa", 1), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("\\a", 2), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("\\a", -1), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("\\\\a", 2), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("\\\\\\a", 4), "Expected to return false for Utils::isEscapedByBackSlash");
        assertFalse(Utils.isEscapedByBackslash("\\\\\\\\a", 6), "Expected to return false for Utils::isEscapedByBackSlash");

        // positive tests
        assertTrue(Utils.isEscapedByBackslash("\\a", 1), "Expected to return true for Utils::isEscapedByBackSlash");
        assertTrue(Utils.isEscapedByBackslash("\\\\\\a", 3), "Expected to return true for Utils::isEscapedByBackSlash");
        assertTrue(Utils.isEscapedByBackslash("\\\\\\\\\\a", 5), "Expected to return true for Utils::isEscapedByBackSlash");
        assertTrue(Utils.isEscapedByBackslash("\\aa", 1), "Expected to return true for Utils::isEscapedByBackSlash");
        assertTrue(Utils.isEscapedByBackslash("a\\a", 2), "Expected to return true for Utils::isEscapedByBackSlash");
        assertTrue(Utils.isEscapedByBackslash("aa\\a", 3), "Expected to return true for Utils::isEscapedByBackSlash");
        assertTrue(Utils.isEscapedByBackslash("aa\\aa", 3), "Expected to return true for Utils::isEscapedByBackSlash");
    }

    @Test
    @Order(38)
    public void testUtils_getBigDecimalStringRepresentation() {
        String testFailureMessage = "Unexpected result from Utils::getBigDecimalStringRepresentation";

        assertEquals("10.0", Utils.getBigDecimalStringRepresentation(new BigDecimal("10")), testFailureMessage);
        assertEquals("10.0", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.0")), testFailureMessage);
        assertEquals("10.0", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.000")), testFailureMessage);
        assertEquals("10.1", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.1")), testFailureMessage);
        assertEquals("10.1", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.10")), testFailureMessage);
        assertEquals("10.001", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.001")), testFailureMessage);
        assertEquals("10.001", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.00100")), testFailureMessage);
        assertEquals("10.111", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.111")), testFailureMessage);
        assertEquals("10.111", Utils.getBigDecimalStringRepresentation(new BigDecimal("10.111000")), testFailureMessage);
    }
}
