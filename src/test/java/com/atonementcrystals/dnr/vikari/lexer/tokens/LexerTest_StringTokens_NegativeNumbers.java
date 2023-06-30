package com.atonementcrystals.dnr.vikari.lexer.tokens;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.lexSingleStatementAsTokens;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.testToken;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_NegativeNumbers {

    @Test
    @Order(1)
    public void testLexer_StringTokens_NegativeNumbers_BasicNegativeInteger() {
        List<String> statement = lexSingleStatementAsTokens("a << -2", 6);

        testToken(statement.get(0), "a");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "-");
        testToken(statement.get(5), "2");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_NegativeNumbers_BasicNegativeDecimal() {
        List<String> statement = lexSingleStatementAsTokens("negative_pi << -3.14", 6);

        testToken(statement.get(0), "negative_pi");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "-");
        testToken(statement.get(5), "3.14");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_NegativeNumbers_BasicNegatedIdentifier() {
        List<String> statement = lexSingleStatementAsTokens("^^ -bar", 4);

        testToken(statement.get(0), "^^");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "-");
        testToken(statement.get(3), "bar");
    }

    @Test
    @Order(4)
    public void testLexer_StringTokens_NegativeNumbers_BasicArithmetic() {
        List<String> statement = lexSingleStatementAsTokens("^^ 1.7 * -[3 + 4 / -2.4]", 19);

        testToken(statement.get(0), "^^");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "1.7");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "*");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "-");
        testToken(statement.get(7), "[");
        testToken(statement.get(8), "3");
        testToken(statement.get(9), " ");
        testToken(statement.get(10), "+");
        testToken(statement.get(11), " ");
        testToken(statement.get(12), "4");
        testToken(statement.get(13), " ");
        testToken(statement.get(14), "/");
        testToken(statement.get(15), " ");
        testToken(statement.get(16), "-");
        testToken(statement.get(17), "2.4");
        testToken(statement.get(18), "]");
    }
}
