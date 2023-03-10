package com.atonement.crystals.dnr.vikari.util;

import com.atonement.crystals.dnr.vikari.error.Vikari_LexerException;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class UtilsTest {

    @Test
    @Order(1)
    public void testUtils_isLongNumber() {
        String integer = "2";
        assertTrue(Utils.isLongNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        integer = "-32";
        assertTrue(Utils.isLongNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        integer = "98209380";
        assertTrue(Utils.isLongNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        String notInteger = "3.14";
        assertFalse(Utils.isLongNumber(notInteger), "Expected decimal number to return false for Utils::isLongNumber.");
        notInteger = "-6.28";
        assertFalse(Utils.isLongNumber(notInteger), "Expected decimal number to return false for Utils::isLongNumber.");
        notInteger = "foo";
        assertFalse(Utils.isLongNumber(notInteger), "Expected non-number to return false for Utils::isLongNumber.");
    }

    @Test
    @Order(2)
    public void testUtils_isDecimalNumber() {
        String notDecimal = "2";
        assertFalse(Utils.isDecimalNumber(notDecimal), "Expected integer number to return false for Utils::isDecimalNumber.");
        notDecimal = "-32";
        assertFalse(Utils.isDecimalNumber(notDecimal), "Expected integer number to return false for Utils::isDecimalNumber.");
        notDecimal = "98209380";
        assertFalse(Utils.isDecimalNumber(notDecimal), "Expected integer number to return false for Utils::isDecimalNumber.");
        String decimal = "3.14";
        assertTrue(Utils.isDecimalNumber(decimal), "Expected decimal number to return true for Utils::isDecimalNumber.");
        decimal = "-6.28";
        assertTrue(Utils.isDecimalNumber(decimal), "Expected decimal number to return true for Utils::isDecimalNumber.");
        decimal = "foo";
        assertFalse(Utils.isDecimalNumber(decimal), "Expected non-number to return false for Utils::isDecimalNumber.");
    }

    @Test
    @Order(3)
    public void testUtils_stripEnclosure() {
        String enclosure = "`foo`";
        String inner = Utils.stripEnclosure(enclosure, "`", "`");
        assertEquals("foo", inner, "String should have backticks removed by Utils::stripEnclosure.");

        enclosure = "``a << 2 - [7 * a]``";
        inner = Utils.stripEnclosure(enclosure, "``", "``");
        assertEquals("a << 2 - [7 * a]", inner, "String should have capture quotations removed by Utils::stripEnclosure.");

        enclosure = "{Janspirical crystal identifier}";
        inner = Utils.stripEnclosure(enclosure, "{", "}");
        assertEquals("Janspirical crystal identifier", inner, "A Janspirical crystal identifier should have its " +
                "curly brackets removed by Utils::stripEnclosure.");

        enclosure = "}Rapnirical crystal identifier{";
        inner = Utils.stripEnclosure(enclosure, "}", "{");
        assertEquals("Rapnirical crystal identifier", inner, "A Rapnirical crystal identifier should have its " +
                "curly brackets removed by Utils::stripEnclosure.");

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
    @Order(4)
    public void testUtils_isWhitespace() {
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
        assertFalse(Utils.isWhitespace("a"), "String with non-whitespace characters should return true for " +
                "Utils::isWhitespace.");
        assertFalse(Utils.isWhitespace(" a\t\t"), "String with non-whitespace characters should return true for " +
                "Utils::isWhitespace.");
    }
}
