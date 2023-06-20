package com.atonementcrystals.dnr.vikari.lexer.stringanalysis;

import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Arithmetic {

    @Test
    @Order(1)
    public void testLexer_StringAnalysis_Arithmetic_BasicArithmeticWithSquareBrackets() {
        String sourceString = "^^ 1.7 * -[3 + 4 / -2.4]";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);

        int expectedStatementCount = 1;
        int actualStatementCount = listOfStatementTokens.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        List<String> statementTokens = listOfStatementTokens.get(0);

        int expectedTokenCount = 19;
        int actualTokenCount = statementTokens.size();
        assertEquals(expectedTokenCount, actualTokenCount, "Unexpected number of tokens.");

        String expectedToken = "^^";
        String actualToken = statementTokens.get(0);
        assertEquals(expectedToken, actualToken, "Expected the return operator.");

        expectedToken = "1.7";
        actualToken = statementTokens.get(2);
        assertEquals(expectedToken, actualToken, "Expected a decimal number literal.");

        expectedToken = "*";
        actualToken = statementTokens.get(4);
        assertEquals(expectedToken, actualToken, "Expected the multiply operator.");

        expectedToken = "-";
        actualToken = statementTokens.get(6);
        assertEquals(expectedToken, actualToken, "Expected the negation operator.");

        expectedToken = "[";
        actualToken = statementTokens.get(7);
        assertEquals(expectedToken, actualToken, "Expected the left square bracket.");

        expectedToken = "3";
        actualToken = statementTokens.get(8);
        assertEquals(expectedToken, actualToken, "Expected an integer number literal.");

        expectedToken = "+";
        actualToken = statementTokens.get(10);
        assertEquals(expectedToken, actualToken, "Expected the addition operator.");

        expectedToken = "4";
        actualToken = statementTokens.get(12);
        assertEquals(expectedToken, actualToken, "Expected the addition operator.");

        expectedToken = "/";
        actualToken = statementTokens.get(14);
        assertEquals(expectedToken, actualToken, "Expected the addition operator.");

        expectedToken = "-";
        actualToken = statementTokens.get(16);
        assertEquals(expectedToken, actualToken, "Expected the addition operator.");

        expectedToken = "2.4";
        actualToken = statementTokens.get(17);
        assertEquals(expectedToken, actualToken, "Expected a negative decimal number literal.");

        expectedToken = "]";
        actualToken = statementTokens.get(18);
        assertEquals(expectedToken, actualToken, "Expected the right square bracket.");
    }
}
