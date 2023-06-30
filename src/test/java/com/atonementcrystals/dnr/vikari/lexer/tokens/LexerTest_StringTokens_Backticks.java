package com.atonementcrystals.dnr.vikari.lexer.tokens;

import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testSyntaxError;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

/**
 * Test that single-backtick quoted identifiers (i.e. `foo`) are properly tokenized by the Lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_Backticks {

    @Test
    @Order(1)
    public void testLexer_StringTokens_SingleBacktickQuotation_SingleCharacterIdentifier() {
        List<String> statement = lexSingleStatementAsTokens("`a` << 2", 5);

        testToken(statement.get(0), "`a`");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "2");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_SingleBacktickQuotation_MultiCharacterIdentifier() {
        List<String> statement = lexSingleStatementAsTokens("`foo` << *", 5);

        testToken(statement.get(0), "`foo`");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "*");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_SingleBacktickQuotation_IdentifierContainingPunctuationSymbols() {
        List<String> statement = lexSingleStatementAsTokens("`~\\*/~` << _", 5);

        testToken(statement.get(0), "`~\\*/~`");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "_");
    }

    @Test
    @Order(4)
    public void testLexer_StringTokens_SingleBacktickQuotation_IdentifierContainingMixOfSymbols() {
        String sourceString = "`~a.~*||_where::foo }{ ^^public ~\\\\.__.//~`:Integer << *";
        List<String> statement = lexSingleStatementAsTokens(sourceString, 7);

        testToken(statement.get(0), "`~a.~*||_where::foo }{ ^^public ~\\\\.__.//~`");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "Integer");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "<<");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "*");
    }

    @Test
    @Order(5)
    public void testLexer_StringTokens_SingleBacktickQuotation_IdentifierContainingNewline() {
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<String>> statements = lexAsTokens("`foo\n`:Integer << *", 2, errorReporter, 2, tokenCounts(1, 1));

        testToken(statements.get(0).get(0), "`foo");
        testToken(statements.get(1).get(0), "`:Integer << *");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();

        String expectedErrorMessage = "Missing closing backtick quotation";
        testSyntaxError(syntaxErrors.get(0), location(0, 0), "`foo", expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(1), location(1, 0), "`:Integer << *", expectedErrorMessage);
    }

    @Test
    @Order(6)
    public void testLexer_StringTokens_SingleBacktickQuotation_IdentifierMissingClosingBacktickQuote() {
        String sourceString = "foo:Integer << `bar";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<String> statement = lexSingleStatementAsTokens(sourceString, 7, errorReporter, 1);

        testToken(statement.get(0), "foo");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "Integer");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "<<");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "`bar");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 15), sourceString, "Missing closing backtick quotation");
    }

    @Test
    @Order(7)
    public void testLexer_StringTokens_SingleBacktickQuotation_MultipleQuotedIdentifiers() {
        List<String> statement = lexSingleStatementAsTokens("`foo`:Integer << `bar`", 7);

        testToken(statement.get(0), "`foo`");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "Integer");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "<<");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "`bar`");
    }

    @Test
    @Order(8)
    public void testLexer_StringTokens_SingleBacktickQuotation_BasicArithmetic() {
        List<String> statement = lexSingleStatementAsTokens("a << [`b` + 2] * `foo`", 15);

        testToken(statement.get(0), "a");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "[");
        testToken(statement.get(5), "`b`");
        testToken(statement.get(6), " ");
        testToken(statement.get(7), "+");
        testToken(statement.get(8), " ");
        testToken(statement.get(9), "2");
        testToken(statement.get(10), "]");
        testToken(statement.get(11), " ");
        testToken(statement.get(12), "*");
        testToken(statement.get(13), " ");
        testToken(statement.get(14), "`foo`");
    }

    @Test
    @Order(9)
    public void testLexer_StringTokens_SingleBacktickQuotation_ContainingSpaceWithNonSpaceCharacters() {
        List<String> statement = lexSingleStatementAsTokens("`foo bar`:Integer << 2", 7);

        testToken(statement.get(0), "`foo bar`");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "Integer");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "<<");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "2");
    }

    @Test
    @Order(10)
    public void testLexer_StringTokens_SingleBacktickQuotation_ContainingTabsShouldFail() {
        String sourceString = "`foo\tbar`:Integer << 2";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<String> statement = lexSingleStatementAsTokens(sourceString, 7, errorReporter, 1);

        testToken(statement.get(0), "`foo\tbar`");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "Integer");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "<<");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "2");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Backtick-quoted identifiers cannot contain tabs.");
    }

    @Test
    @Order(11)
    public void testLexer_StringTokens_SingleBacktickQuotation_OnlyWhitespaceCharactersShouldFail() {
        // ------------
        // single space
        // ------------
        List<String> statement = lexSingleStatementAsTokens("space:Character << ` `", 7);

        testToken(statement.get(0), "space");
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "Character");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "<<");
        testToken(statement.get(5), " ");
        testToken(statement.get(6), "` `");

        // ---------------
        // multiple spaces
        // ---------------
        String sourceString = "`   ` << *";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatementAsTokens(sourceString, 5, errorReporter, 1);

        testToken(statement.get(0), "`   `");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "*");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Backtick-quoted identifiers cannot " +
                "contain only whitespace.");

        // ----------
        // single tab
        // ----------
        sourceString = "`\t` << *";

        errorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatementAsTokens(sourceString, 5, errorReporter, 1);

        testToken(statement.get(0), "`\t`");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "*");

        syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Backtick-quoted identifiers cannot " +
                "contain only whitespace.");

        // -------------
        // multiple tabs
        // -------------
        sourceString = "`\t\t` << *";

        errorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatementAsTokens(sourceString, 5, errorReporter, 1);

        testToken(statement.get(0), "`\t\t`");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "*");

        syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Backtick-quoted identifiers cannot " +
                "contain only whitespace.");

        // ----------------------
        // mix of spaces and tabs
        // ----------------------
        sourceString = "` \t  \t\t   ` << *";

        errorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatementAsTokens(sourceString, 5, errorReporter, 1);

        testToken(statement.get(0), "` \t  \t\t   `");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "*");

        syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Backtick-quoted identifiers cannot " +
                "contain only whitespace.");
    }

    @Test
    @Order(12)
    public void testLexer_StringTokens_BacktickCharacterLiteral() {
        List<String> statement = lexSingleStatementAsTokens("`\\``", 1);
        testToken(statement.get(0), "`\\``");
    }
}
