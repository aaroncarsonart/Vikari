package com.atonementcrystals.dnr.vikari.lexer.tokens;

import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test that decimal number literal values (i.e. "3.14") are properly tokenized by the Lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_DecimalNumbers {

    @Test
    @Order(1)
    public void testLexer_StringTokens_DecimalNumbers_BasicAssignment() {
        String sourceString = "pi << 3.14";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 5;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "3.14";
        String actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_DecimalNumbers_ComplexArithmetic() {
        String sourceString = "a << 6.999 - [5.2 / 9001.0]";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 15;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "6.999";
        String actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");

        expectedToken = "5.2";
        actualToken = statementTokens.get(9);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");

        expectedToken = "9001.0";
        actualToken = statementTokens.get(13);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_DecimalNumbers_AcrossMultipleLines() {
        String sourceString = "foo << 3.14\n" +
                ":foo + 6.28";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> firstStatementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 5;
        int actualTokenCount = firstStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "3.14";
        String actualToken = firstStatementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");

        List<String> secondStatementTokens = listOfStatementTokens.get(1);

        expectedTokenCount = 6;
        actualTokenCount = secondStatementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        expectedToken = "6.28";
        actualToken = secondStatementTokens.get(5);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");
    }
}
