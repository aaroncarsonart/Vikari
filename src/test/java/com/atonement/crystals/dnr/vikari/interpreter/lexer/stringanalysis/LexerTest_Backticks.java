package com.atonement.crystals.dnr.vikari.interpreter.lexer.stringanalysis;

import com.atonement.crystals.dnr.vikari.error.SyntaxError;
import com.atonement.crystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonement.crystals.dnr.vikari.interpreter.Lexer;
import com.atonement.crystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.File;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test that single-back-tick quoted identifiers (i.e. `foo`) are properly
 * collapsed down into a singular string after the lexical analysis step.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Backticks {
    private static final CoordinatePair COORDINATE_PAIR_ZERO_ZERO = new CoordinatePair(0, 0);

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
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for backtick quoted identifier across 2 lines.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 2;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = "`foo";
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("backtick"), "Unexpected syntax error message.");

        // Syntax Error 2
        syntaxError = syntaxErrors.get(1);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(1, 0);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = "`:Integer << *";
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("backtick"), "Unexpected syntax error message.");
    }

    @Test
    @Order(6)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_IdentifierMissingClosingBacktickQuote() {
        String sourceString = "foo:Integer << `bar";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for missing a closing backtick.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 1;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = new CoordinatePair(0, 15);
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = "foo:Integer << `bar";
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("backtick"), "Unexpected syntax error message.");
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
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for a backtick-quoted identifier containing a " +
                "tab.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 1;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = new CoordinatePair(0, 1);
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = sourceString;
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("tab"), "Unexpected syntax error message.");
    }

    @Test
    @Order(11)
    public void testLexer_StringAnalysis_SingleBacktickQuotation_OnlyWhitespaceCharactersShouldFail() {
        // ------------
        // single space
        // ------------
        String sourceString = "space:Character << ` `";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertFalse(errorReporter.hasErrors(), "A space character literal should not cause a syntax error");

        // ---------------
        // multiple spaces
        // ---------------
        sourceString = "`   ` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for a backtick-quoted identifier containing " +
                "only whitespace.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 1;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = new CoordinatePair(0, 1);
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = sourceString;
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("whitespace"), "Unexpected syntax error message.");

        // ----------
        // single tab
        // ----------
        sourceString = "`\t` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for a backtick-quoted identifier containing a " +
                "tab.");

        syntaxErrors = errorReporter.getSyntaxErrors();
        expectedSize = 1;
        actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        syntaxError = syntaxErrors.get(0);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(0, 1);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = sourceString;
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("whitespace"), "Unexpected syntax error message.");

        // -------------
        // multiple tabs
        // -------------
        sourceString = "`\t\t` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for a backtick-quoted identifier containing " +
                "tabs.");

        syntaxErrors = errorReporter.getSyntaxErrors();
        expectedSize = 1;
        actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        syntaxError = syntaxErrors.get(0);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(0, 1);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = sourceString;
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("whitespace"), "Unexpected syntax error message.");

        // ----------------------
        // mix of spaces and tabs
        // ----------------------
        sourceString = "` \t  \t\t   ` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for a backtick-quoted identifier containing a " +
                "tab.");

        syntaxErrors = errorReporter.getSyntaxErrors();
        expectedSize = 1;
        actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        syntaxError = syntaxErrors.get(0);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(0, 1);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = sourceString;
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("whitespace"), "Unexpected syntax error message.");
    }
}
