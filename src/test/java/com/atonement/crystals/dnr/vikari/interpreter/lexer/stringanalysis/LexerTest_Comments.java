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
 * Test that comment crystal enclosed text (i.e. ~:foo:~) are properly
 * collapsed down into a singular string after the lexical analysis step.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Comments {
    public static final CoordinatePair COORDINATE_PAIR_ZERO_ZERO = new CoordinatePair(0, 0);

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_CommentEnclosure_BasicOneLineComment() {
        String sourceString = "~:This is a comment.:~";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 1;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = sourceString;
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");
    }

    @Test
    @Order(2)
    public void testLexer_StringAnalysis_CommentEnclosure_BasicTwoLineComment() {
        String sourceString = "~:This is a comment\n" +
                "across two lines.:~";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 2;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        // Test the first statement's tokens
        List<String> firstStatementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 1;
        int actualTokenCount = firstStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "~:This is a comment";
        String actualToken = firstStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");

        // Test the second statement's tokens
        List<String> secondStatementTokens = listOfStatementTokens.get(1);

        expectedTokenCount = 1;
        actualTokenCount = secondStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        expectedToken = "across two lines.:~";
        actualToken = secondStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");
    }

    @Test
    @Order(3)
    public void testLexer_StringAnalysis_CommentEnclosure_BasicThreeLineComment() {
        String sourceString = "~:This is a comment\n" +
                "across three lines\n" +
                "to test enclosures.:~";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 3;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        // Test the first statement's tokens
        List<String> firstStatementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 1;
        int actualTokenCount = firstStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "~:This is a comment";
        String actualToken = firstStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");

        // Test the second statement's tokens
        List<String> secondStatementTokens = listOfStatementTokens.get(1);

        expectedTokenCount = 1;
        actualTokenCount = secondStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        expectedToken = "across three lines";
        actualToken = secondStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");


        // Test the third statement's tokens
        List<String> thirdStatementTokens = listOfStatementTokens.get(2);

        expectedTokenCount = 1;
        actualTokenCount = thirdStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        expectedToken = "to test enclosures.:~";
        actualToken = thirdStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");
    }

    @Test
    @Order(4)
    public void testLexer_StringAnalysis_CommentEnclosure_EndOfLineComment() {
        String sourceString = "a << 314 ~:`a` is approximately: [pi * 100].:~";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 7;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "~:`a` is approximately: [pi * 100].:~";
        String actualToken = statementTokens.get(6);
        assertEquals(expectedToken, actualToken, "Malformed comment.");
    }

    @Test
    @Order(5)
    public void testLexer_StringAnalysis_CommentEnclosure_TwoLineComment_AfterStatement_OnFirstLine() {
        String sourceString = "a << 314 ~:`a` is approximately: [pi * 100].\n" +
                "But sometimes, we prefer to use tau instead!:~";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 2;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        // first statement
        List<String> firstStatementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 7;
        int actualTokenCount = firstStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "~:`a` is approximately: [pi * 100].";
        String actualToken = firstStatementTokens.get(6);
        assertEquals(expectedToken, actualToken, "Malformed comment.");

        // second statement
        List<String> secondStatementTokens = listOfStatementTokens.get(1);

        expectedTokenCount = 1;
        actualTokenCount = secondStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        expectedToken = "But sometimes, we prefer to use tau instead!:~";
        actualToken = secondStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");
    }

    @Test
    @Order(6)
    public void testLexer_StringAnalysis_CommentEnclosure_TwoLineComment_BeforeStatement_OnSecondLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].\n" +
                "But sometimes, we prefer to use tau instead!:~ a << 314";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 2;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        // first statement
        List<String> firstStatementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 1;
        int actualTokenCount = firstStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "~:`a` is approximately: [pi * 100].";
        String actualToken = firstStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");

        // second statement
        List<String> secondStatementTokens = listOfStatementTokens.get(1);

        expectedTokenCount = 7;
        actualTokenCount = secondStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        expectedToken = "But sometimes, we prefer to use tau instead!:~";
        actualToken = secondStatementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed comment.");
    }

    @Test
    @Order(7)
    public void testLexer_StringAnalysis_CommentEnclosure_ErrorHandlingForUnclosedComment_SingleLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for missing a closing comment suffix.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 1;
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

        String expectedLine = sourceString;
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("comment suffix"), "Unexpected syntax error message.");
    }

    @Test
    @Order(8)
    public void testLexer_StringAnalysis_CommentEnclosure_ErrorHandlingForUnclosedComment_MultiLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].\n" +
                "However, I forgot to close this comment!";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for missing a closing comment suffix.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 1;
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

        String expectedLine = "~:`a` is approximately: [pi * 100].";
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("comment suffix"), "Unexpected syntax error message.");
    }
}
