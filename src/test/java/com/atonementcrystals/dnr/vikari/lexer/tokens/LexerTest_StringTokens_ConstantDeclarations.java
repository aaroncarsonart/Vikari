package com.atonementcrystals.dnr.vikari.lexer.tokens;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

/**
 * Test that constant declarations (i.e. "{foo}") are properly tokenized by the Lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_ConstantDeclarations {

    @Test
    @Order(1)
    public void testLexer_StringTokens_ConstantDeclarations_BasicIdentifier() {
        List<String> statement = lexSingleStatementAsTokens("{foo}", 3);

        testToken(statement.get(0), "{");
        testToken(statement.get(1), "foo");
        testToken(statement.get(2), "}");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_ConstantDeclarations_BasicIdentifierAssignment() {
        List<String> statement = lexSingleStatementAsTokens("{foo}:Integer << 2", 9);

        testToken(statement.get(0), "{");
        testToken(statement.get(1), "foo");
        testToken(statement.get(2), "}");
        testToken(statement.get(3), ":");
        testToken(statement.get(4), "Integer");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "<<");
        testToken(statement.get(7), " ");
        testToken(statement.get(8), "2");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_ConstantDeclarations_IdentifierContainingBackticks() {
        List<String> statement = lexSingleStatementAsTokens("{`An Identifier`}:Integer << 2", 9);

        testToken(statement.get(0), "{");
        testToken(statement.get(1), "`An Identifier`");
        testToken(statement.get(2), "}");
        testToken(statement.get(3), ":");
        testToken(statement.get(4), "Integer");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "<<");
        testToken(statement.get(7), " ");
        testToken(statement.get(8), "2");
    }
}
