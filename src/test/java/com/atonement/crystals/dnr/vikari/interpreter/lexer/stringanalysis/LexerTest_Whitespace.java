package com.atonement.crystals.dnr.vikari.interpreter.lexer.stringanalysis;

import com.atonement.crystals.dnr.vikari.interpreter.Lexer;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Whitespace {

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_Whitespace_CollapsionOfSpaces() {
        String sourceString = "    a <<  *";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 6;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "    ";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 4 spaces.");

        expectedToken = " ";
        actualToken = statementTokens.get(2);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 1 space.");

        expectedToken = "  ";
        actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 2 spaces.");
    }

    @Test
    @Order(2)
    public void testLexer_StringAnalysis_Whitespace_CollapsionOfTabs() {
        // NOTE: Code should never be written this way.
        // But still, sequential tabs should collapse together!
        String sourceString = "\t\t\ta\t<<\t\t*";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 6;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "\t\t\t";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 3 tabs.");

        expectedToken = "\t";
        actualToken = statementTokens.get(2);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 1 tab.");

        expectedToken = "\t\t";
        actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 2 tabs.");
    }

    @Test
    @Order(3)
    public void testLexer_StringAnalysis_Whitespace_CollapsionOfTabsAndSpaces() {
        // NOTE: Code should never be written this way.
        // But still, arbitrary combinations of tabs
        // and spaces should collapse together!
        String sourceString = "\t\t  a << \t 2   +\t \t7\t/  9";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 14;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "\t\t  ";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 2 tabs and 2 spaces.");

        expectedToken = " ";
        actualToken = statementTokens.get(2);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 1 space.");

        expectedToken = " \t ";
        actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 1 space, 1 tab, and " +
                "1 space.");

        expectedToken = "   ";
        actualToken = statementTokens.get(6);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 3 spaces.");

        expectedToken = "\t \t";
        actualToken = statementTokens.get(8);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 1 tab, 1 space, and " +
                "1 tab.");

        expectedToken = "\t";
        actualToken = statementTokens.get(10);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 1 tab.");

        expectedToken = "  ";
        actualToken = statementTokens.get(12);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 2 spaces.");
    }

    @Test
    @Order(4)
    public void testLexer_StringAnalysis_Whitespace_AtEndOfLine() {
        String sourceString = "a << *   ";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 6;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "   ";
        String actualToken = statementTokens.get(5);
        assertEquals(expectedToken, actualToken, "Expected a whitespace token consisting of 3 spaces.");
    }
}
