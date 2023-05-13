package com.atonementcrystals.dnr.vikari.lexer.stringanalysis;

import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test that constant declarations (i.e. "{foo}") are properly
 * lex during the first step of lexical analysis.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_ConstantDeclarations {

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_ConstantDeclarations_BasicIdentifier() {
        String sourceString = "{foo}";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        List<String> expectedTokens = Arrays.asList("{", "foo", "}");
        int expectedTokenCount = expectedTokens.size();
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        for (int i = 0; i < expectedTokenCount; i++) {
            String expectedToken = expectedTokens.get(i);
            String actualToken = statementTokens.get(i);
            assertEquals(expectedToken, actualToken, "Malformed constant declaration.");
        }
    }

    @Test
    @Order(2)
    public void testLexer_StringAnalysis_ConstantDeclarations_BasicIdentifierAssignment() {
        String sourceString = "{foo}:Integer << 2";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        List<String> expectedTokens = Arrays.asList("{", "foo", "}", ":", "Integer", " ", "<<", " ", "2");
        int expectedTokenCount = expectedTokens.size();
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        for (int i = 0; i < expectedTokenCount; i++) {
            String expectedToken = expectedTokens.get(i);
            String actualToken = statementTokens.get(i);
            assertEquals(expectedToken, actualToken, "Malformed constant declaration.");
        }
    }

    @Test
    @Order(3)
    public void testLexer_StringAnalysis_ConstantDeclarations_IdentifierContainingBackticks() {
        String sourceString = "{`An Identifier`}:Integer << 2";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        List<String> expectedTokens = Arrays.asList("{", "`An Identifier`", "}", ":", "Integer", " ", "<<", " ", "2");
        int expectedTokenCount = expectedTokens.size();
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        for (int i = 0; i < expectedTokenCount; i++) {
            String expectedToken = expectedTokens.get(i);
            String actualToken = statementTokens.get(i);
            assertEquals(expectedToken, actualToken, "Malformed constant declaration.");
        }
    }
}
