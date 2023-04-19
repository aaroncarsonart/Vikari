package com.atonement.crystals.dnr.vikari.interpreter.lexer.stringanalysis;

import com.atonement.crystals.dnr.vikari.error.Vikari_LexerException;
import com.atonement.crystals.dnr.vikari.interpreter.Lexer;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test that single-back-tick quoted identifiers (i.e. `foo`) are properly
 * collapsed down into a singular string after the lexical analysis step.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Backticks {

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_SingleCharacterIdentifer() {
        String sourceString = "`a` << 2";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokensCount = 5;
        int actualTokensCount = statementTokens.size();
        assertEquals(expectedTokensCount, actualTokensCount, "Unexpected number of tokens.");

        String expectedToken = "`a`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(2)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_MultiCharacterIdentifer() {
        String sourceString = "`foo` << *";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokensCount = 5;
        int actualTokensCount = statementTokens.size();
        assertEquals(expectedTokensCount, actualTokensCount, "Unexpected number of tokens.");

        String expectedToken = "`foo`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(3)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_IdentiferContainingPunctuationSymbols() {
        String sourceString = "`~\\*/~` << _";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokensCount = 5;
        int actualTokensCount = statementTokens.size();
        assertEquals(expectedTokensCount, actualTokensCount, "Unexpected number of tokens.");

        String expectedToken = "`~\\*/~`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(4)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_IdentiferContainingMixOfSymbols() {
        String sourceString = "`~a.~*||_where::foo }{ ^^public ~\\\\.__.//~`:Integer << *";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokensCount = 7;
        int actualTokensCount = statementTokens.size();
        assertEquals(expectedTokensCount, actualTokensCount, "Unexpected number of tokens.");

        String expectedToken = "`~a.~*||_where::foo }{ ^^public ~\\\\.__.//~`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(5)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_IdentifierContainingNewline() {
        String sourceString = "`foo\n`:Integer << *";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Single-backtick-quoted identifiers containing a newline should fail.");
        } catch (Vikari_LexerException e) {
            // success
        }
    }

    @Test
    @Order(6)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_IdentifierMissingClosingBacktickQuote() {
        String sourceString = "`foo:Integer << *";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Single-backtick quotations missing a closing backtick should fail.");
        } catch (Vikari_LexerException e) {
            // success
        }
    }

    @Test
    @Order(7)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_MultipleQuotedIdentifiers() {
        String sourceString = "`foo`:Integer << `bar`";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokensCount = 7;
        int actualTokensCount = statementTokens.size();
        assertEquals(expectedTokensCount, actualTokensCount, "Unexpected number of tokens.");

        String expectedToken = "`foo`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");

        expectedToken = "`bar`";
        actualToken = statementTokens.get(6);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(8)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_BasicArithmetic() {
        String sourceString = "a << [`b` + 2] * `foo`";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 15;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "`b`";
        String actualToken = statementTokens.get(5);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");

        expectedToken = "`foo`";
        actualToken = statementTokens.get(14);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(9)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_ContainingSpaceWithNonSpaceCharacters() {
        String sourceString = "`foo bar`:Integer << 2";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokensCount = 7;
        int actualTokensCount = statementTokens.size();
        assertEquals(expectedTokensCount, actualTokensCount, "Unexpected number of tokens.");

        String expectedToken = "`foo bar`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier containing a space.");
    }

    @Test
    @Order(10)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_ContainingTabsShouldFail() {
        String sourceString = "`foo\tbar`:Integer << 2";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing a tab character.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("tab"), "Unexpected error message: " + e.getErrorMessage());
        }
    }

    @Test
    @Order(11)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_OnlyWhitespaceCharactersShouldFail() {
        // single space
        String sourceString = "` ` << 2";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // multiple spaces
        sourceString = "`   ` << *";
        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // single tab
        sourceString = "`\t` << *";
        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // multiple tabs
        sourceString = "`\t\t` << *";
        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // mix of spaces and tabs
        sourceString = "` \t  \t\t   ` << *";
        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        try {
            lexer.collapseTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }
    }
}
