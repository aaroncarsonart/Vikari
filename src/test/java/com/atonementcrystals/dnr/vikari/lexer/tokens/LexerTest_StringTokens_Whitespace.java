package com.atonementcrystals.dnr.vikari.lexer.tokens;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.lexSingleStatementAsTokens;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.testToken;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_Whitespace {

    @Test
    @Order(1)
    public void testLexer_StringTokens_Whitespace_CollapsionOfSpaces() {
        List<String> statement = lexSingleStatementAsTokens("    a <<  1", 6);

        testToken(statement.get(0), "    ");
        testToken(statement.get(1), "a");
        testToken(statement.get(2), " ");
        testToken(statement.get(3), "<<");
        testToken(statement.get(4), "  ");
        testToken(statement.get(5), "1");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_Whitespace_CollapsionOfTabs() {
        // NOTE: Code should never be written this way.
        // But still, sequential tabs should collapse together!

        List<String> statement = lexSingleStatementAsTokens("\t\t\ta\t<<\t\t*", 6);

        testToken(statement.get(0), "\t\t\t");
        testToken(statement.get(1), "a");
        testToken(statement.get(2), "\t");
        testToken(statement.get(3), "<<");
        testToken(statement.get(4), "\t\t");
        testToken(statement.get(5), "*");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_Whitespace_CollapsionOfTabsAndSpaces() {
        // NOTE: Code should never be written this way.
        // But still, arbitrary combinations of tabs
        // and spaces should collapse together!

        List<String> statement = lexSingleStatementAsTokens("\t\t  a << \t 2   +\t \t7\t/  9", 14);

        testToken(statement.get(0), "\t\t  ");
        testToken(statement.get(1), "a");
        testToken(statement.get(2), " ");
        testToken(statement.get(3), "<<");
        testToken(statement.get(4), " \t ");
        testToken(statement.get(5), "2");
        testToken(statement.get(6), "   ");
        testToken(statement.get(7), "+");
        testToken(statement.get(8), "\t \t");
        testToken(statement.get(9), "7");
        testToken(statement.get(10), "\t");
        testToken(statement.get(11), "/");
        testToken(statement.get(12), "  ");
        testToken(statement.get(13), "9");
    }

    @Test
    @Order(4)
    public void testLexer_StringTokens_Whitespace_AtEndOfLine() {
        List<String> statement = lexSingleStatementAsTokens("a << 1   ", 6);

        testToken(statement.get(0), "a");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "1");
        testToken(statement.get(5), "   ");
    }
}
