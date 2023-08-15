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

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class UtilsTest {

    @Test
    @Order(1)
    public void testUtils_isIntegerNumber() {
        // positive tests
        String integer = "2";
        assertTrue(Utils.isIntegerNumber(integer), "Expected integer number to return true for Utils::isIntegerNumber.");
        integer = "-32";
        assertTrue(Utils.isIntegerNumber(integer), "Expected integer number to return true for Utils::isIntegerNumber.");
        integer = "98209380";
        assertTrue(Utils.isIntegerNumber(integer), "Expected integer number to return true for Utils::isIntegerNumber.");

        // negative tests
        String longInteger = "22L";
        assertTrue(Utils.isLongIntegerNumber(longInteger), "Expected long literal to return false for Utils::isIntegerNumber.");
        longInteger = "913l";
        assertTrue(Utils.isLongIntegerNumber(longInteger), "Expected long literal to return false for Utils::isIntegerNumber.");
        longInteger = "4000000000";
        assertTrue(Utils.isLongIntegerNumber(longInteger), "Expected long literal to return false for Utils::isIntegerNumber.");

        String notInteger = "3.14";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected decimal number to return false for Utils::isIntegerNumber.");
        notInteger = "-6.28";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected decimal number to return false for Utils::isIntegerNumber.");
        notInteger = "foo";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected non-number to return false for Utils::isIntegerNumber.");
    }

    @Test
    @Order(2)
    public void testUtils_isLongNumber() {
        // positive tests
        String integer = "2";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        integer = "-32";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        integer = "98209380";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        integer = "22L";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected long literal to return true for Utils::isLongNumber.");
        integer = "913l";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected long literal to return true for Utils::isLongNumber.");
        integer = "4000000000";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected long literal to return true for Utils::isLongNumber.");

        // negative tests
        String notInteger = "3.14";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected decimal number to return false for Utils::isLongNumber.");
        notInteger = "-6.28";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected decimal number to return false for Utils::isLongNumber.");
        notInteger = "foo";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected non-number to return false for Utils::isLongNumber.");
    }

    @Test
    @Order(3)
    public void testUtils_isFloatNumber() {
        // positive tests
        String notDecimal = "2";
        assertTrue(Utils.isFloatNumber(notDecimal), "Expected integer number to return true for Utils::isFloatNumber.");
        notDecimal = "-32";
        assertTrue(Utils.isFloatNumber(notDecimal), "Expected integer number to return true for Utils::isFloatNumber.");
        notDecimal = "98209380";
        assertTrue(Utils.isFloatNumber(notDecimal), "Expected integer number to return true for Utils::isFloatNumber.");

        String decimal = "3.14";
        assertTrue(Utils.isFloatNumber(decimal), "Expected decimal number to return true for Utils::isFloatNumber.");
        decimal = "-6.28";
        assertTrue(Utils.isFloatNumber(decimal), "Expected decimal number to return true for Utils::isFloatNumber.");

        String floatLiteral = "2.0F";
        assertTrue(Utils.isFloatNumber(floatLiteral), "Expected float literal to return true for Utils::isFloatNumber.");
        floatLiteral = "22.7f";
        assertTrue(Utils.isFloatNumber(floatLiteral), "Expected float literal to return true for Utils::isFloatNumber.");

        // negative tests
        String doubleLiteral = "2.0D";
        assertFalse(Utils.isFloatNumber(doubleLiteral), "Expected double literal to return false for Utils::isFloatNumber.");
        doubleLiteral = "22.7d";
        assertFalse(Utils.isFloatNumber(doubleLiteral), "Expected double literal to return false for Utils::isFloatNumber.");
        String nonNumber = "foo";
        assertFalse(Utils.isFloatNumber(nonNumber), "Expected non-number to return false for Utils::isFloatNumber.");
    }

    @Test
    @Order(4)
    public void testUtils_isDoubleNumber() {
        // positive tests
        String notDecimal = "2";
        assertTrue(Utils.isDoubleNumber(notDecimal), "Expected integer number to return true for Utils::isDoubleNumber.");
        notDecimal = "-32";
        assertTrue(Utils.isDoubleNumber(notDecimal), "Expected integer number to return true for Utils::isDoubleNumber.");
        notDecimal = "98209380";
        assertTrue(Utils.isDoubleNumber(notDecimal), "Expected integer number to return true for Utils::isDoubleNumber.");

        String decimal = "3.14";
        assertTrue(Utils.isDoubleNumber(decimal), "Expected decimal number to return true for Utils::isDoubleNumber.");
        decimal = "-6.28";
        assertTrue(Utils.isDoubleNumber(decimal), "Expected decimal number to return true for Utils::isDoubleNumber.");

        String doubleLiteral = "2.0D";
        assertTrue(Utils.isDoubleNumber(doubleLiteral), "Expected double literal to return true for Utils::isDoubleNumber.");
        doubleLiteral = "22.7d";
        assertTrue(Utils.isDoubleNumber(doubleLiteral), "Expected double literal to return true for Utils::isDoubleNumber.");

        // negative tests
        String floatLiteral = "2.0F";
        assertFalse(Utils.isDoubleNumber(floatLiteral), "Expected float literal to return false for Utils::isDoubleNumber.");
        floatLiteral = "22.7f";
        assertFalse(Utils.isDoubleNumber(floatLiteral), "Expected float literal to return false for Utils::isDoubleNumber.");
        decimal = "foo";
        assertFalse(Utils.isDoubleNumber(decimal), "Expected non-number to return false for Utils::isDoubleNumber.");
    }

    @Test
    @Order(5)
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
    @Order(6)
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
    @Order(7)
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
    @Order(8)
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
    @Order(9)
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
    @Order(10)
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
    @Order(11)
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
    @Order(12)
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
    @Order(13)
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
    @Order(14)
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
    @Order(15)
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
    @Order(16)
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
    @Order(17)
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
    @Order(18)
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
    @Order(19)
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
    @Order(20)
    public void testHasLongSuffix() {
        assertTrue(Utils.hasLongSuffix("22L"), "Expected literal to have a long suffix.");
        assertTrue(Utils.hasLongSuffix("7l"), "Expected literal to have a long suffix.");
        assertFalse(Utils.hasLongSuffix("512B"), "Expected big integer literal to have a long suffix.");
        assertFalse(Utils.hasLongSuffix("3.14b"), "Expected big decimal literal to have a long suffix.");
        assertFalse(Utils.hasLongSuffix("5"), "Expected an integer literal to not have a big suffix.");
        assertFalse(Utils.hasLongSuffix("5f"), "Expected a float literal to not have a big suffix.");
        assertFalse(Utils.hasLongSuffix("5d"), "Expected a double literal to not have a big suffix.");
        assertFalse(Utils.hasLongSuffix("3.14"), "Expected an double literal to not have a big suffix.");
    }

    @Test
    @Order(21)
    public void testHasBigSuffix() {
        assertTrue(Utils.hasBigSuffix("5B"), "Expected literal to have a big suffix.");
        assertTrue(Utils.hasBigSuffix("5b"), "Expected literal to have a big suffix.");
        assertTrue(Utils.hasBigSuffix("3.14B"), "Expected literal to have a big suffix.");
        assertTrue(Utils.hasBigSuffix("3.14b"), "Expected literal to have a big suffix.");
        assertFalse(Utils.hasBigSuffix("5"), "Expected an integer literal to not have a big suffix.");
        assertFalse(Utils.hasBigSuffix("5f"), "Expected a float literal to not have a big suffix.");
        assertFalse(Utils.hasBigSuffix("5d"), "Expected a double literal to not have a big suffix.");
        assertFalse(Utils.hasBigSuffix("3.14"), "Expected an double literal to not have a big suffix.");
    }

    @Test
    @Order(22)
    public void testTrimLastCharacter() {
        assertEquals("5", Utils.trimLastCharacter("5B"), "Unexpected trim result.");
        assertEquals("5", Utils.trimLastCharacter("5b"), "Unexpected trim result.");
        assertEquals("3.14", Utils.trimLastCharacter("3.14B"), "Unexpected trim result.");
        assertEquals("3.14", Utils.trimLastCharacter("3.14b"), "Unexpected trim result.");
        assertEquals("22", Utils.trimLastCharacter("22L"), "Unexpected trim result.");
        assertEquals("7", Utils.trimLastCharacter("7l"), "Unexpected trim result.");
    }

    @Test
    @Order(23)
    public void testIsBigInteger() {
        assertTrue(Utils.isBigIntegerNumber("5"), "Expected literal to be a big decimal number.");
        assertTrue(Utils.isBigIntegerNumber("7B"), "Expected literal to be a big decimal number.");
        assertTrue(Utils.isBigIntegerNumber("22b"), "Expected literal to be a big decimal number.");
    }

    @Test
    @Order(24)
    public void testIsBigDecimal() {
        assertTrue(Utils.isBigDecimalNumber("5"), "Expected literal to be a big decimal number.");
        assertTrue(Utils.isBigDecimalNumber("3.14"), "Expected literal to be a big decimal number.");
        assertTrue(Utils.isBigDecimalNumber("7B"), "Expected literal to be a big decimal number.");
        assertTrue(Utils.isBigDecimalNumber("7.0b"), "Expected literal to be a big decimal number.");
    }

    @Test
    @Order(25)
    public void testIsValidDecimalFractionalPart() {
        assertTrue(Utils.isValidDecimalFractionalPart("5"), "Expected to be a valid decimal fractional part.");
        assertTrue(Utils.isValidDecimalFractionalPart("5f"), "Expected to be a valid decimal fractional part.");
        assertTrue(Utils.isValidDecimalFractionalPart("5F"), "Expected to be a valid decimal fractional part.");
        assertTrue(Utils.isValidDecimalFractionalPart("5d"), "Expected to be a valid decimal fractional part.");
        assertTrue(Utils.isValidDecimalFractionalPart("5D"), "Expected to be a valid decimal fractional part.");
        assertTrue(Utils.isValidDecimalFractionalPart("5b"), "Expected to be a valid decimal fractional part.");
        assertTrue(Utils.isValidDecimalFractionalPart("5B"), "Expected to be a valid decimal fractional part.");
        assertFalse(Utils.isValidDecimalFractionalPart("5.0"), "Expected to not be a valid decimal fractional part.");
        assertFalse(Utils.isValidDecimalFractionalPart("5.0B"), "Expected to not be a valid decimal fractional part.");
        assertFalse(Utils.isValidDecimalFractionalPart("7Y"), "Expected to not be a valid decimal fractional part.");
        assertFalse(Utils.isValidDecimalFractionalPart("22BB"), "Expected to not be a valid decimal fractional part.");
    }

    @Test
    @Order(26)
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
    @Order(27)
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
    @Order(28)
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
    @Order(29)
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
}
