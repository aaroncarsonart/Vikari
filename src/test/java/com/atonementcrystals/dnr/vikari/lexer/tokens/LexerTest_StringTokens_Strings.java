package com.atonementcrystals.dnr.vikari.lexer.tokens;

import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testSyntaxError;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

/**
 * Test that string literal identifiers (i.e. ``foo``) are properly tokenized by the Lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_Strings {

    @Test
    @Order(1)
    public void testLexer_StringTokens_CaptureQuotations_BasicStringLiteral() {
        String sourceString = "``a:Integer << 2, :a + 5, _``";
        List<String> statement = lexSingleStatementAsTokens(sourceString, 1);
        testToken(statement.get(0), sourceString);
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_CaptureQuotations_BasicStringLiteralAssignment() {
        List<String> statement = lexSingleStatementAsTokens("a << ``b:Integer << 2, :b + 5, _``", 5);

        testToken(statement.get(0), "a");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "``b:Integer << 2, :b + 5, _``");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_CaptureQuotations_BasicTwoLineString() {
        String sourceString = "``This is a string which spans \n" +
                                "across two individual lines.``";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));

        testToken(statements.get(0).get(0), "``This is a string which spans ");
        testToken(statements.get(1).get(0), "across two individual lines.``");
    }

    @Test
    @Order(4)
    public void testLexer_StringTokens_CaptureQuotations_BasicThreeLineString() {
        String sourceString = "``This string needs to span \n" +
                "across 3 lines. So I'm going to \n" +
                "ensure that it most certainly does.``";

        List<List<String>> statements = lexAsTokens(sourceString, 3, tokenCounts(1, 1, 1));

        testToken(statements.get(0).get(0), "``This string needs to span ");
        testToken(statements.get(1).get(0), "across 3 lines. So I'm going to ");
        testToken(statements.get(2).get(0), "ensure that it most certainly does.``");
    }

    @Test
    @Order(5)
    public void testLexer_StringTokens_CaptureQuotations_MultiLineStringAfterCodeStatement() {
        String sourceString = "foo << ``This is a string being assigned \n" +
                "to an identifier named: `foo`.``";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(5, 1));

        testToken(statements.get(0).get(0), "foo");
        testToken(statements.get(0).get(1), " ");
        testToken(statements.get(0).get(2), "<<");
        testToken(statements.get(0).get(3), " ");

        testToken(statements.get(0).get(4), "``This is a string being assigned ");
        testToken(statements.get(1).get(0), "to an identifier named: `foo`.``");
    }

    @Test
    @Order(6)
    public void testLexer_StringTokens_CaptureQuotations_StringLiteralContainingCodeWithOtherEnclosures() {
        String sourceString = "foo << ``{bar} << 2, :foo.`baz`, buzz << _``";
        List<String> statement = lexSingleStatementAsTokens(sourceString, 5);

        testToken(statement.get(0), "foo");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "``{bar} << 2, :foo.`baz`, buzz << _``");
    }

    @Test
    @Order(7)
    public void testLexer_StringTokens_CaptureQuotations_StringLiteralContainingCodeAcrossMultipleLines() {
        String sourceString = "function << () :: ``{foo} << AtonementCrystal!\n" +
                "{bar} << 2\n" +
                ":foo.`baz`\n" +
                "buzz << _``!";

        List<List<String>> statements = lexAsTokens(sourceString, 4, tokenCounts(10, 1, 1, 2));

        testToken(statements.get(0).get(0), "function");
        testToken(statements.get(0).get(1), " ");
        testToken(statements.get(0).get(2), "<<");
        testToken(statements.get(0).get(3), " ");
        testToken(statements.get(0).get(4), "(");
        testToken(statements.get(0).get(5), ")");
        testToken(statements.get(0).get(6), " ");
        testToken(statements.get(0).get(7), "::");
        testToken(statements.get(0).get(8), " ");

        testToken(statements.get(0).get(9), "``{foo} << AtonementCrystal!");
        testToken(statements.get(1).get(0), "{bar} << 2");
        testToken(statements.get(2).get(0), ":foo.`baz`");
        testToken(statements.get(3).get(0), "buzz << _``");

        testToken(statements.get(3).get(1), "!");
    }

    @Test
    @Order(8)
    public void testLexer_StringTokens_CaptureQuotations_ErrorHandlingForUnclosedString_SingleLine() {
        String sourceString = "``This is a malformed string literal.";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<String> statement = lexSingleStatementAsTokens(sourceString, 1, errorReporter, 1);

        testToken(statement.get(0), sourceString);

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Missing closing capture quotation");
    }

    @Test
    @Order(9)
    public void testLexer_StringTokens_CaptureQuotations_ErrorHandlingForUnclosedString_MultiLine() {
        String sourceString = "``This is a malformed string literal \n" +
                "because it has no ending capture quotation!";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<String>> statements = lexAsTokens(sourceString, 2, errorReporter, 1, tokenCounts(1, 1));

        testToken(statements.get(0).get(0), "``This is a malformed string literal ");
        testToken(statements.get(1).get(0), "because it has no ending capture quotation!");

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();

        String expectedLine = "``This is a malformed string literal ";
        testSyntaxError(syntaxErrors.get(0), location(0, 0), expectedLine, "Missing closing capture quotation");
    }
}
