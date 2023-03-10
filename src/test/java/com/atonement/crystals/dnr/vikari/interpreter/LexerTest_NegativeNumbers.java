package com.atonement.crystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_NegativeNumbers {

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_NegativeNumbers_BasicNegativeInteger() {
        String sourceString = "a << -2";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 5;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens: " + statementTokens);

        String expectedToken = "-2";
        String actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");
    }

    @Test
    @Order(2)
    public void testLexer_StringAnalysis_NegativeNumbers_BasicNegativeDecimal() {
        String sourceString = "negative_pi << -3.14";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 5;
        int actualTokenCount = statementTokens.size();
        System.out.println(statementTokens);
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "-3.14";
        String actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Malformed decimal literal.");
    }

    @Test
    @Order(3)
    public void testLexer_StringAnalysis_NegativeNumbers_BasicNegatedIdentifier() {
        String sourceString = "^^ -bar";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 4;
        int actualTokenCount = statementTokens.size();
        System.out.println(statementTokens);
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "-";
        String actualToken = statementTokens.get(2);
        assertEquals(expectedToken, actualToken, "Malformed negation operator.");

        System.out.println(statementTokens);

        expectedToken = "bar";
        actualToken = statementTokens.get(3);
        assertEquals(expectedToken, actualToken, "Malformed identifier.");
    }

    @Test
    @Order(4)
    public void testLexer_StringAnalysis_NegativeNumbers_BasicArithmetic() {
        String sourceString = "^^ 1.7 * -[3 + 4 / -2.4]";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.readStringAsBasicStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseEnclosuresOfStringTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 18;
        int actualTokenCount = statementTokens.size();
        System.out.println(statementTokens);
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "-";
        String actualToken = statementTokens.get(6);
        assertEquals(expectedToken, actualToken, "Malformed negation operator.");

        System.out.println(statementTokens);

        expectedToken = "-2.4";
        actualToken = statementTokens.get(16);
        assertEquals(expectedToken, actualToken, "Malformed negative decimal number literal.");
    }
}
