package com.atonement.crystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class LexerTest {

    private final ByteArrayOutputStream testOutputStream = new ByteArrayOutputStream();
    private final PrintStream systemOutputStream = System.out;

//    /**
//     * Redirect standard output to an output stream whose contents
//     * can be directly analyzed for the purposes of testing.
//     */
//    @BeforeEach
//    public void setupOutputStreams() {
//        System.setOut(new PrintStream(testOutputStream));
//    }

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_CaptureQuotationEnclosure_MultiLineStatement() {
        String sourceString = "``a:Integer << 2, :a + 5, _``";

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
        assertEquals(expectedToken, actualToken, "Malformed string literal.");
    }

    @Test
    @Order(2)
    public void testLexer_StringAnalysis_CaptureQuotationEnclosure_stringLiteralAssignment() {
        String sourceString = "a << ``b:Integer << 2, :b + 5, _``";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 5;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String actualCommentToken = statementTokens.get(0);
        String expectedCommentToken = sourceString;
        String stringLiteral = statementTokens.get(4);
        assertEquals("``b:Integer << 2, :b + 5, _``", stringLiteral, "Malformed string literal.");
    }

    @Test
    @Order(3)
    public void testLexer_StringAnalysis_BacktickQuotationEnclosure_basicSingleCharacterIdentifier_Assignment() {
        String sourceString = "`a` << 2";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 5;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "`a`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(4)
    public void testLexer_StringAnalysis_BacktickQuotationEnclosure_basicMultiCharacterIdentifier_Assignment() {
        String sourceString = "`identifier` << *";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 5;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "`identifier`";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");
    }

    @Test
    @Order(5)
    public void testLexer_StringAnalysis_BacktickQuotationEnclosure_basicArithmetic() {
        String sourceString = "a << [`b` + 2] * 'foo''";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 15;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "`a`";
        String actualToken = statementTokens.get(6);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");

        expectedToken = "`foo`";
        actualToken = statementTokens.get(14);
        assertEquals(expectedToken, actualToken, "Malformed backtick-quoted identifier.");

    }

//    /**
//     * Restore the original print stream for standard output after finished testing.
//     */
//
//    @AfterEach
//    public void restoreOutputStreams() {
//        System.setOut(systemOutputStream);
//    }
}