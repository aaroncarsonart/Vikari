package com.atonementcrystals.dnr.vikari.lexer.tokens;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.lexSingleStatementAsTokens;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.testToken;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_Arithmetic {

    @Test
    @Order(1)
    public void testLexer_StringTokens_Arithmetic_BasicArithmeticWithSquareBrackets() {
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
