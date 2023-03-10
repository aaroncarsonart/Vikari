package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.error.Vikari_LexerException;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test that Janspirical identifiers (i.e. "{foo}") are properly collapsed
 * down into a singular string after the lexical analysis step.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_JanspiricalCrystals {

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_JanspiricalCrystal_BasicIdentifier() {
        String sourceString = "{I am a Janspirical crystal's identifier.}";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 1;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = sourceString;
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed Janspirical crystal identifier.");
    }

    @Test
    @Order(2)
    public void testLexer_StringAnalysis_JanspiricalCrystal_BasicIdentifierAssignment() {
        String sourceString = "{foo}:Integer << 2";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 7;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "{foo}";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed Janspirical crystal identifier.");
    }

    @Test
    @Order(3)
    public void testLexer_StringAnalysis_JanspiricalCrystal_IdentifierContainingBackticks() {
        String sourceString = "{`foo`}:Integer << 2";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        try {
            lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("Backticks"), "Unexpected error message: " + e.getErrorMessage());
            return; // pass
        }
        fail("Expected Vikari_LexerException for backticks within Janspirical crystal enclosure.");
    }

    @Test
    @Order(4)
    public void testLexer_StringAnalysis_JanspiricalCrystal_ContainingTabsShouldFail() {
        String sourceString = "{foo\tbar}:Integer << 2";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        try {
            lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing a tab character.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("Janspirical"), "Unexpected error message: " + e.getErrorMessage());
            assertTrue(e.getErrorMessage().contains("tab"), "Unexpected error message: " + e.getErrorMessage());
        }
    }

    @Test
    @Order(5)
    public void testLexer_StringAnalysis_JanspiricalCrystal_OnlyWhitespaceCharactersShouldFail() {
        // single space
        String sourceString = "{ } << 2";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        try {
            lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("Janspirical"), "Unexpected error message: " + e.getErrorMessage());
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // multiple spaces
        sourceString = "{   } << *";
        listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        try {
            lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("Janspirical"), "Unexpected error message: " + e.getErrorMessage());
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // single tab
        sourceString = "{\t} << *";
        listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        try {
            lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("Janspirical"), "Unexpected error message: " + e.getErrorMessage());
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // multiple tabs
        sourceString = "{\t\t} << *";
        listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        try {
            lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("Janspirical"), "Unexpected error message: " + e.getErrorMessage());
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }

        // mix of spaces and tabs
        sourceString = "{ \t  \t\t   } << *";
        listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        try {
            lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);
            fail("Expected Vikari_LexerException for backticks containing only whitespace characters.");
        } catch (Vikari_LexerException e) {
            assertTrue(e.getErrorMessage().contains("Janspirical"), "Unexpected error message: " + e.getErrorMessage());
            assertTrue(e.getErrorMessage().contains("whitespace"), "Unexpected error message: " + e.getErrorMessage());
        }
    }
}
