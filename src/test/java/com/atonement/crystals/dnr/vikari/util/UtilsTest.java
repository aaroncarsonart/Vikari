package com.atonement.crystals.dnr.vikari.util;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.Keyword;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
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
        // positive tests
        String integer = "2";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        integer = "-32";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        integer = "98209380";
        assertTrue(Utils.isLongIntegerNumber(integer), "Expected integer number to return true for Utils::isLongNumber.");
        String notInteger = "3.14";

        // negative tests
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected decimal number to return false for Utils::isLongNumber.");
        notInteger = "-6.28";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected decimal number to return false for Utils::isLongNumber.");
        notInteger = "foo";
        assertFalse(Utils.isLongIntegerNumber(notInteger), "Expected non-number to return false for Utils::isLongNumber.");
    }

    @Test
    @Order(2)
    public void testUtils_isDecimalNumber() {
        // negative tests
        String notDecimal = "2";
        assertFalse(Utils.isDecimalNumber(notDecimal), "Expected integer number to return false for Utils::isDecimalNumber.");
        notDecimal = "-32";
        assertFalse(Utils.isDecimalNumber(notDecimal), "Expected integer number to return false for Utils::isDecimalNumber.");
        notDecimal = "98209380";
        assertFalse(Utils.isDecimalNumber(notDecimal), "Expected integer number to return false for Utils::isDecimalNumber.");

        // positive tests
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
    @Order(4)
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
    @Order(5)
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
    @Order(6)
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
    @Order(6)
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
    @Order(7)
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
    @Order(8)
    public void testUtils_isStringLiteral() {
        // positive tests
        String stringLiteral = "``foo``";
        assertTrue(Utils.isStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isStringLiteral.");
        stringLiteral = "``int:Integer << 5``";
        assertTrue(Utils.isStringLiteral(stringLiteral), "A string literal should return true for " +
                "Utils::isStringLiteral.");

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
    }

    @Test
    @Order(9)
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
    @Order(9)
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
    }

    @Test
    @Order(9)
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
    @Order(10)
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
    @Order(11)
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
    }

    @Test
    @Order(11)
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
    @Order(12)
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
    @Order(13)
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
}
