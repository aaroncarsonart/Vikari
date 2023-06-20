package com.atonementcrystals.dnr.vikari.lexer.stringanalysis;

import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.File;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Test that comment crystals (i.e. ~:foo:~) are properly tokenized by the Lexer.
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

    @Test
    @Order(9)
    public void testLexer_StringAnalysis_Comments_QuotedSuffixToken_AfterOneBackslash() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\:~:~a", 2);
        testToken(statement.get(0), "~:This is a comment.\\:~:~");
        testToken(statement.get(1), "a");
    }

    @Test
    @Order(10)
    public void testLexer_StringAnalysis_Comments_UnQuotedSuffixToken_AfterTwoBackslashes() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\\\:~:~a", 4);
        testToken(statement.get(0), "~:This is a comment.\\\\:~");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "~");
        testToken(statement.get(3), "a");
    }

    @Test
    @Order(11)
    public void testLexer_StringAnalysis_Comments_QuotedSuffixToken_AfterThreeBackslashes() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\\\\\:~:~", 1);
        testToken(statement.get(0), "~:This is a comment.\\\\\\:~:~");
    }

    @Test
    @Order(12)
    public void testLexer_StringAnalysis_Comments_UnQuotedSuffixToken_AfterFourBackslashes() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\\\\\\\:~:~a", 4);
        testToken(statement.get(0), "~:This is a comment.\\\\\\\\:~");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "~");
        testToken(statement.get(3), "a");
    }

    @Test
    @Order(13)
    public void testLexer_StringAnalysis_MultLineComment_QuotedSuffixToken_BeforeEndOfLine() {
        String sourceString = """
            ~:This suffix token is quoted: \\:~ and so,
            the comment can then span across two lines.:~
            """;

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testToken(statements.get(0).get(0), "~:This suffix token is quoted: \\:~ and so,");
        testToken(statements.get(1).get(0), "the comment can then span across two lines.:~");
    }

    @Test
    @Order(14)
    public void testLexer_StringAnalysis_MultLineComment_QuotedSuffixToken_BeforeEndOfLine_OneCharacterAfter() {
        String sourceString = """
            ~:This suffix token is quoted \\:~,
            and so the comment can then span across two lines.:~
            """;

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testToken(statements.get(0).get(0), "~:This suffix token is quoted \\:~,");
        testToken(statements.get(1).get(0), "and so the comment can then span across two lines.:~");
    }

    @Test
    @Order(15)
    public void testLexer_StringAnalysis_MultLineComment_QuotedSuffixToken_AtEndOfLine() {
        String sourceString = """
            ~:This suffix token is quoted: \\:~
            and so, the comment can then span across two lines.:~
            """;

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testToken(statements.get(0).get(0), "~:This suffix token is quoted: \\:~");
        testToken(statements.get(1).get(0), "and so, the comment can then span across two lines.:~");
    }

    @Test
    @Order(16)
    public void testLexer_StringAnalysis_CommentBeforePrintStatement() {
        List<String> statement = lexSingleStatementAsTokens("~:Comment.:~:", 2);
        testToken(statement.get(0), "~:Comment.:~");
        testToken(statement.get(1), ":");
    }

    @Test
    @Order(17)
    public void testLexer_StringAnalysis_CommentAfterPrintStatement() {
        List<String> statement = lexSingleStatementAsTokens(":~:Comment.:~", 2);
        testToken(statement.get(0), ":");
        testToken(statement.get(1), "~:Comment.:~");
    }

    @Test
    @Order(18)
    public void testLexer_StringAnalysis_CommentBeforeTypeLabel() {
        List<String> statement = lexSingleStatementAsTokens("foo~:Comment.:~:Integer", 4);
        testToken(statement.get(0), "foo");
        testToken(statement.get(1), "~:Comment.:~");
        testToken(statement.get(2), ":");
        testToken(statement.get(3), "Integer");
    }

    @Test
    @Order(19)
    public void testLexer_StringAnalysis_CommentAfterTypeLabel() {
        List<String> statement = lexSingleStatementAsTokens("foo:~:Comment.:~Integer", 4);
        testToken(statement.get(0), "foo");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "~:Comment.:~");
        testToken(statement.get(3), "Integer");
    }

    @Test
    @Order(20)
    public void testLexer_StringAnalysis_MultiLineCommentBeforePrintStatement() {
        List<List<String>> statements = lexAsTokens("~:Multi-line\ncomment.:~:", 2, tokenCounts(1, 2));
        testToken(statements.get(0).get(0), "~:Multi-line");
        testToken(statements.get(1).get(0), "comment.:~");
        testToken(statements.get(1).get(1), ":");
    }

    @Test
    @Order(21)
    public void testLexer_StringAnalysis_MultiLineCommentAfterPrintStatement() {
        List<List<String>> statements = lexAsTokens(":~:Multi-line\ncomment.:~", 2, tokenCounts(2, 1));
        testToken(statements.get(0).get(0), ":");
        testToken(statements.get(0).get(1), "~:Multi-line");
        testToken(statements.get(1).get(0), "comment.:~");
    }

    @Test
    @Order(22)
    public void testLexer_StringAnalysis_MultiLineCommentBeforeTypeLabel() {
        List<List<String>> statements = lexAsTokens("foo~:Multi-line\ncomment.:~:Integer", 2, tokenCounts(2, 3));
        testToken(statements.get(0).get(0), "foo");
        testToken(statements.get(0).get(1), "~:Multi-line");
        testToken(statements.get(1).get(0), "comment.:~");
        testToken(statements.get(1).get(1), ":");
        testToken(statements.get(1).get(2), "Integer");
    }

    @Test
    @Order(23)
    public void testLexer_StringAnalysis_MultiLineCommentAfterTypeLabel() {
        List<List<String>> statements = lexAsTokens("foo:~:Multi-line\ncomment.:~Integer", 2, tokenCounts(3, 2));
        testToken(statements.get(0).get(0), "foo");
        testToken(statements.get(0).get(1), ":");
        testToken(statements.get(0).get(2), "~:Multi-line");
        testToken(statements.get(1).get(0), "comment.:~");
        testToken(statements.get(1).get(1), "Integer");
    }

    @Test
    @Order(24)
    public void testLexer_StringAnalysis_TwoSequentialComments() {
        List<String> statement = lexSingleStatementAsTokens("~:Comment 1.:~~:Comment 2.:~", 2);
        testToken(statement.get(0), "~:Comment 1.:~");
        testToken(statement.get(1), "~:Comment 2.:~");
    }

    @Test
    @Order(25)
    public void testLexer_StringAnalysis_TwoSequentialMultiLineComments() {
        String sourceString = "~:Multi-line\ncomment 1.:~~:Multi-line\ncomment 2.:~";
        List<List<String>> statements = lexAsTokens(sourceString, 3, tokenCounts(1, 2, 1));
        testToken(statements.get(0).get(0), "~:Multi-line");
        testToken(statements.get(1).get(0), "comment 1.:~");
        testToken(statements.get(1).get(1), "~:Multi-line");
        testToken(statements.get(2).get(0), "comment 2.:~");
    }
}
