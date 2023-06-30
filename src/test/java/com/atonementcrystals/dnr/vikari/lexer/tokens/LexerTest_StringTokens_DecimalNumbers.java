package com.atonementcrystals.dnr.vikari.lexer.tokens;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

/**
 * Test that decimal number literal values (i.e. "3.14") are properly tokenized by the Lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_DecimalNumbers {

    @Test
    @Order(1)
    public void testLexer_StringTokens_DecimalNumbers_BasicAssignment() {
        List<String> statement = lexSingleStatementAsTokens("pi << 3.14", 5);

        testToken(statement.get(0), "pi");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "3.14");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_DecimalNumbers_ComplexArithmetic() {
        List<String> statement = lexSingleStatementAsTokens("a << 6.999 - [5.2 / 9001.0]", 15);

        testToken(statement.get(0), "a");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "6.999");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "-");
        testToken(statement.get(7), " ");
        testToken(statement.get(8), "[");
        testToken(statement.get(9), "5.2");
        testToken(statement.get(10), " ");
        testToken(statement.get(11), "/");
        testToken(statement.get(12), " ");
        testToken(statement.get(13), "9001.0");
        testToken(statement.get(14), "]");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_DecimalNumbers_AcrossMultipleLines() {
        String sourceString = "foo << 3.14\n:foo + 6.28";
        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(5, 6));

        testToken(statements.get(0).get(0), "foo");
        testToken(statements.get(0).get(1), " ");
        testToken(statements.get(0).get(2), "<<");
        testToken(statements.get(0).get(3), " ");
        testToken(statements.get(0).get(4), "3.14");

        testToken(statements.get(1).get(0), ":");
        testToken(statements.get(1).get(1), "foo");
        testToken(statements.get(1).get(2), " ");
        testToken(statements.get(1).get(3), "+");
        testToken(statements.get(1).get(4), " ");
        testToken(statements.get(1).get(5), "6.28");
    }
}
