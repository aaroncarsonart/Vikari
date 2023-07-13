package com.atonementcrystals.dnr.vikari.lexer.tokens;

import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.Collections;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testSyntaxError;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

/**
 * Test that comment crystals (i.e. ~:foo:~) are properly tokenized by the Lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_Comments {

    private void testSingleLineComment(String sourceString, CoordinatePair... errorLocations) {
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        int expectedErrorCount = errorLocations.length;

        List<String> statement = lexSingleStatementAsTokens(sourceString, 1, errorReporter, expectedErrorCount);

        CommentTokenType commentTokenType = CommentTokenType.SINGLE_LINE;
        if (errorLocations.length > 0) {
            commentTokenType = CommentTokenType.START;
        }
        testComment(statement.get(0), sourceString.length(), commentTokenType);

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        for (int i = 0; i < expectedErrorCount; i++) {
            VikariError syntaxError = syntaxErrors.get(i);
            CoordinatePair expecedLocation = errorLocations[i];
            testSyntaxError(syntaxError, expecedLocation, sourceString, "Missing comment suffix token");
        }
    }

    private void testCommentSyntaxError(VikariError syntaxError, CoordinatePair expectedLocation, String expectedLine) {
        testSyntaxError(syntaxError, expectedLocation, expectedLine, "Missing comment suffix token");
    }

    @Test
    @Order(1)
    public void testLexer_StringTokens_CommentEnclosure_BasicOneLineComment() {
        testSingleLineComment("~:This is a comment.:~");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_CommentEnclosure_BasicTwoLineComment() {
        List<List<String>> statements = lexAsTokens("""
                ~:This is a comment
                across two lines.:~""", 2, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:This is a comment".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "across two lines.:~".length(), CommentTokenType.END);
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_CommentEnclosure_BasicThreeLineComment() {
        List<List<String>> statements = lexAsTokens("""
                ~:This is a comment
                across three lines
                to test enclosures.:~""", 3, tokenCounts(1, 1, 1));

        testComment(statements.get(0).get(0), "~:This is a comment".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "across three lines".length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), "to test enclosures.:~".length(), CommentTokenType.END);
    }

    @Test
    @Order(4)
    public void testLexer_StringTokens_CommentEnclosure_EndOfLineComment() {
        String sourceString = "a << 314 ~:`a` is approximately: [pi * 100].:~";

        List<String> statement = lexSingleStatementAsTokens(sourceString, 7);

        testToken(statement.get(0), "a");
        testToken(statement.get(1), " ");
        testToken(statement.get(2), "<<");
        testToken(statement.get(3), " ");
        testToken(statement.get(4), "314");
        testToken(statement.get(5), " ");
        testComment(statement.get(6), "~:`a` is approximately: [pi * 100].:~".length(), CommentTokenType.SINGLE_LINE);
    }

    @Test
    @Order(5)
    public void testLexer_StringTokens_CommentEnclosure_TwoLineComment_AfterStatement_OnFirstLine() {
        String sourceString = "a << 314 ~:`a` is approximately: [pi * 100].\n" +
                "But sometimes, we prefer to use tau instead!:~";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(7, 1));

        testToken(statements.get(0).get(0), "a");
        testToken(statements.get(0).get(1), " ");
        testToken(statements.get(0).get(2), "<<");
        testToken(statements.get(0).get(3), " ");
        testToken(statements.get(0).get(4), "314");
        testToken(statements.get(0).get(5), " ");
        testComment(statements.get(0).get(6), "~:`a` is approximately: [pi * 100].".length(), CommentTokenType.START);

        testComment(statements.get(1).get(0), "But sometimes, we prefer to use tau instead!:~".length(), CommentTokenType.END);
    }

    @Test
    @Order(6)
    public void testLexer_StringTokens_CommentEnclosure_TwoLineComment_BeforeStatement_OnSecondLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].\n" +
                "But sometimes, we prefer to use tau instead!:~ a << 314";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 7));

        testComment(statements.get(0).get(0), "~:`a` is approximately: [pi * 100].".length(), CommentTokenType.START);

        testComment(statements.get(1).get(0), "But sometimes, we prefer to use tau instead!:~".length(), CommentTokenType.END);
        testToken(statements.get(1).get(1), " ");
        testToken(statements.get(1).get(2), "a");
        testToken(statements.get(1).get(3), " ");
        testToken(statements.get(1).get(4), "<<");
        testToken(statements.get(1).get(5), " ");
        testToken(statements.get(1).get(6), "314");
    }

    @Test
    @Order(7)
    public void testLexer_StringTokens_CommentEnclosure_ErrorHandlingForUnclosedComment_SingleLine() {
        testSingleLineComment("~:`a` is approximately: [pi * 100].", location(0, 0));
    }

    @Test
    @Order(8)
    public void testLexer_StringTokens_CommentEnclosure_ErrorHandlingForUnclosedComment_MultiLine() {
        String sourceString = """
                ~:`a` is approximately: [pi * 100].
                However, I forgot to close this comment!""";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<String>> statements = lexAsTokens(sourceString, 2, errorReporter, 1, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:`a` is approximately: [pi * 100].".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "However, I forgot to close this comment!".length(), CommentTokenType.MIDDLE);

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), "~:`a` is approximately: [pi * 100].");
    }

    // Quoted prefix token tests.

    @Test
    @Order(9)
    public void testLexer_StringTokens_Comments_QuotedPrefixToken_AfterOneBackslash() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\~::~a", 2);
        testComment(statement.get(0), "~:This is a comment.\\~::~".length(), CommentTokenType.SINGLE_LINE);
        testToken(statement.get(1), "a");
    }

    @Test
    @Order(10)
    public void testLexer_StringTokens_Comments_UnQuotedPrefixToken_AfterTwoBackslashes() {
        testSingleLineComment("~:This is a comment.\\\\~::~a", location(0, 0));
    }

    @Test
    @Order(11)
    public void testLexer_StringTokens_Comments_QuotedPrefixToken_AfterThreeBackslashes() {
        testSingleLineComment("~:This is a comment.\\\\\\~::~");
    }

    @Test
    @Order(12)
    public void testLexer_StringTokens_Comments_UnQuotedPrefixToken_AfterFourBackslashes() {
        testSingleLineComment("~:This is a comment.\\\\\\\\~::~a", location(0, 0));
    }

    @Test
    @Order(13)
    public void testLexer_StringTokens_MultLineComment_QuotedPrefixToken_BeforeEndOfLine() {
        String sourceString = """
            ~:This suffix token is quoted: \\~: and so,
            the comment can then span across two lines.:~""";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:This suffix token is quoted: \\~: and so,".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "the comment can then span across two lines.:~".length(), CommentTokenType.END);
    }

    @Test
    @Order(14)
    public void testLexer_StringTokens_MultLineComment_QuotedPrefixToken_BeforeEndOfLine_OneCharacterAfter() {
        String sourceString = """
            ~:This suffix token is quoted \\~:,
            and so the comment can then span across two lines.:~""";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:This suffix token is quoted \\~:,".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "and so the comment can then span across two lines.:~".length(), CommentTokenType.END);

    }

    @Test
    @Order(15)
    public void testLexer_StringTokens_MultLineComment_QuotedPrefixToken_AtEndOfLine() {
        String sourceString = """
            ~:This suffix token is quoted: \\~:
            and so, the comment can then span across two lines.:~""";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:This suffix token is quoted: \\~:".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "and so, the comment can then span across two lines.:~".length(), CommentTokenType.END);
    }

    // Quoted suffix token tests.

    @Test
    @Order(16)
    public void testLexer_StringTokens_Comments_QuotedSuffixToken_AfterOneBackslash() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\:~:~a", 2);
        testComment(statement.get(0), "~:This is a comment.\\:~:~".length(), CommentTokenType.SINGLE_LINE);
        testToken(statement.get(1), "a");
    }

    @Test
    @Order(17)
    public void testLexer_StringTokens_Comments_UnQuotedSuffixToken_AfterTwoBackslashes() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\\\:~:~a", 4);
        testComment(statement.get(0), "~:This is a comment.\\\\:~".length(), CommentTokenType.SINGLE_LINE);
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "~");
        testToken(statement.get(3), "a");
    }

    @Test
    @Order(18)
    public void testLexer_StringTokens_Comments_QuotedSuffixToken_AfterThreeBackslashes() {
        testSingleLineComment("~:This is a comment.\\\\\\:~:~");
    }

    @Test
    @Order(19)
    public void testLexer_StringTokens_Comments_UnQuotedSuffixToken_AfterFourBackslashes() {
        List<String> statement = lexSingleStatementAsTokens("~:This is a comment.\\\\\\\\:~:~a", 4);
        testComment(statement.get(0), "~:This is a comment.\\\\\\\\:~".length(), CommentTokenType.SINGLE_LINE);
        testToken(statement.get(1), ":");
        testToken(statement.get(2), "~");
        testToken(statement.get(3), "a");
    }

    @Test
    @Order(20)
    public void testLexer_StringTokens_MultLineComment_QuotedSuffixToken_BeforeEndOfLine() {
        String sourceString = """
            ~:This suffix token is quoted: \\:~ and so,
            the comment can then span across two lines.:~""";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:This suffix token is quoted: \\:~ and so,".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "the comment can then span across two lines.:~".length(), CommentTokenType.END);
    }

    @Test
    @Order(21)
    public void testLexer_StringTokens_MultLineComment_QuotedSuffixToken_BeforeEndOfLine_OneCharacterAfter() {
        String sourceString = """
            ~:This suffix token is quoted \\:~,
            and so the comment can then span across two lines.:~""";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:This suffix token is quoted \\:~,".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "and so the comment can then span across two lines.:~".length(), CommentTokenType.END);

    }

    @Test
    @Order(22)
    public void testLexer_StringTokens_MultLineComment_QuotedSuffixToken_AtEndOfLine() {
        String sourceString = """
            ~:This suffix token is quoted: \\:~
            and so, the comment can then span across two lines.:~""";

        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));

        testComment(statements.get(0).get(0), "~:This suffix token is quoted: \\:~".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "and so, the comment can then span across two lines.:~".length(), CommentTokenType.END);
    }

    @Test
    @Order(23)
    public void testLexer_StringTokens_CommentBeforePrintStatement() {
        List<String> statement = lexSingleStatementAsTokens("~:Comment.:~:", 2);
        testComment(statement.get(0), "~:Comment.:~".length(), CommentTokenType.SINGLE_LINE);
        testToken(statement.get(1), ":");
    }

    @Test
    @Order(24)
    public void testLexer_StringTokens_CommentAfterPrintStatement() {
        List<String> statement = lexSingleStatementAsTokens(":~:Comment.:~", 2);
        testToken(statement.get(0), ":");
        testComment(statement.get(1), "~:Comment.:~".length(), CommentTokenType.SINGLE_LINE);
    }

    @Test
    @Order(25)
    public void testLexer_StringTokens_CommentBeforeTypeLabel() {
        List<String> statement = lexSingleStatementAsTokens("foo~:Comment.:~:Integer", 4);
        testToken(statement.get(0), "foo");
        testComment(statement.get(1), "~:Comment.:~".length(), CommentTokenType.SINGLE_LINE);
        testToken(statement.get(2), ":");
        testToken(statement.get(3), "Integer");
    }

    @Test
    @Order(26)
    public void testLexer_StringTokens_CommentAfterTypeLabel() {
        List<String> statement = lexSingleStatementAsTokens("foo:~:Comment.:~Integer", 4);
        testToken(statement.get(0), "foo");
        testToken(statement.get(1), ":");
        testComment(statement.get(2), "~:Comment.:~".length(), CommentTokenType.SINGLE_LINE);
        testToken(statement.get(3), "Integer");
    }

    @Test
    @Order(27)
    public void testLexer_StringTokens_MultiLineCommentBeforePrintStatement() {
        List<List<String>> statements = lexAsTokens("~:Multi-line\ncomment.:~:", 2, tokenCounts(1, 2));
        testComment(statements.get(0).get(0), "~:Multi-line".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "comment.:~".length(), CommentTokenType.END);
        testToken(statements.get(1).get(1), ":");
    }

    @Test
    @Order(28)
    public void testLexer_StringTokens_MultiLineCommentAfterPrintStatement() {
        List<List<String>> statements = lexAsTokens(":~:Multi-line\ncomment.:~", 2, tokenCounts(2, 1));
        testToken(statements.get(0).get(0), ":");
        testComment(statements.get(0).get(1), "~:Multi-line".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "comment.:~".length(), CommentTokenType.END);
    }

    @Test
    @Order(29)
    public void testLexer_StringTokens_MultiLineCommentBeforeTypeLabel() {
        List<List<String>> statements = lexAsTokens("foo~:Multi-line\ncomment.:~:Integer", 2, tokenCounts(2, 3));
        testToken(statements.get(0).get(0), "foo");
        testComment(statements.get(0).get(1), "~:Multi-line".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "comment.:~".length(), CommentTokenType.END);
        testToken(statements.get(1).get(1), ":");
        testToken(statements.get(1).get(2), "Integer");
    }

    @Test
    @Order(30)
    public void testLexer_StringTokens_MultiLineCommentAfterTypeLabel() {
        List<List<String>> statements = lexAsTokens("foo:~:Multi-line\ncomment.:~Integer", 2, tokenCounts(3, 2));
        testToken(statements.get(0).get(0), "foo");
        testToken(statements.get(0).get(1), ":");
        testComment(statements.get(0).get(2), "~:Multi-line".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "comment.:~".length(), CommentTokenType.END);
        testToken(statements.get(1).get(1), "Integer");
    }

    @Test
    @Order(31)
    public void testLexer_StringTokens_TwoSequentialComments() {
        List<String> statement = lexSingleStatementAsTokens("~:Comment 1.:~~:Comment 2.:~", 2);
        testComment(statement.get(0), "~:Comment 1.:~".length(), CommentTokenType.SINGLE_LINE);
        testComment(statement.get(1), "~:Comment 2.:~".length(), CommentTokenType.SINGLE_LINE);
    }

    @Test
    @Order(32)
    public void testLexer_StringTokens_TwoSequentialMultiLineComments() {
        String sourceString = "~:Multi-\nline\ncomment\n1.:~~:Multi-\nline\ncomment\n2.:~";
        List<List<String>> statements = lexAsTokens(sourceString, 7, tokenCounts(1, 1, 1, 2, 1, 1, 1));
        testComment(statements.get(0).get(0), "~:Multi-".length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), "line".length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), "comment".length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), "1.:~".length(), CommentTokenType.END);

        testComment(statements.get(3).get(1), "~:Multi-".length(), CommentTokenType.START);
        testComment(statements.get(4).get(0), "line".length(), CommentTokenType.MIDDLE);
        testComment(statements.get(5).get(0), "comment".length(), CommentTokenType.MIDDLE);
        testComment(statements.get(6).get(0), "2.:~".length(), CommentTokenType.END);
    }

    // Nested comments.

    @Test
    @Order(33)
    public void testLexer_StringTokens_NestedComment_SingleLineCommentsInSingleLineComments() {
        testSingleLineComment("~:Outer comment. ~:Inner comment.:~ :~");
        testSingleLineComment("~:Outer comment.~:Inner comment.:~:~");

        testSingleLineComment("~: ~:Inner comment.:~ Outer comment.:~");
        testSingleLineComment("~:~:Inner comment.:~Outer comment.:~");

        testSingleLineComment("~:Outer ~:Inner comment.:~ comment.:~");
        testSingleLineComment("~:Outer~:Inner comment.:~comment.:~");

        testSingleLineComment("~:First comment. ~:Second comment. ~:Third comment.:~ :~ :~");
        testSingleLineComment("~:First comment.~:Second comment.~:Third comment.:~:~:~");

        testSingleLineComment("~: ~: ~:First comment.:~ Second comment.:~ Third comment.:~");
        testSingleLineComment("~:~:~:First comment.:~Second comment.:~Third comment.:~");

        testSingleLineComment("~:First ~:Second ~:Third comment.:~ comment.:~ comment.:~");
        testSingleLineComment("~:First~:Second~:Third comment.:~comment.:~comment.:~");
    }

    @Test
    @Order(34)
    public void testLexer_StringTokens_NestedComment_SingleLineCommentsInSingleLineComments_ErrorCases() {
        testSingleLineComment("~:", location(0, 0));
        testSingleLineComment("~:~:", location(0, 0), location(0, 2));
        testSingleLineComment("~:~:~:", location(0, 0), location(0, 2), location(0, 4));
        testSingleLineComment("~:~::~", location(0, 0));
        testSingleLineComment("~:~:~::~", location(0, 0), location(0, 2));
        testSingleLineComment("~:~::~~:", location(0, 0), location(0, 6));
        testSingleLineComment("~:~::~~::~:", location(0, 0));
        testSingleLineComment("~:~:~::~:~", location(0, 0));
        testSingleLineComment("~:~:~:~::~:~:~", location(0, 0));
        testSingleLineComment("~:~:~::~~::~:~", location(0, 0));

        // Ensure closing tokens aren't parsed from previously consumed tokens.
        testSingleLineComment("~:~", location(0, 0));
        testSingleLineComment("~:~:~", location(0, 0), location(0, 2));
        testSingleLineComment("~:~:~:~", location(0, 0), location(0, 2), location(0, 4));

        testSingleLineComment("~:Outer comment. ~:Inner comment.", location(0, 0), location(0, 17));
        testSingleLineComment("~:Outer comment. ~:Inner comment.:~", location(0, 0));

        testSingleLineComment("~:c1 ~:c2 ~:c3 ~:c4", location(0, 0), location(0, 5), location(0, 10), location(0, 15));
        testSingleLineComment("~:c1 ~:c2 ~:c3 ~:c4:~", location(0, 0), location(0, 5), location(0, 10));
        testSingleLineComment("~:c1 ~:c2 ~:c3:~ ~:c4", location(0, 0), location(0, 5), location(0, 17));
        testSingleLineComment("~:c1 ~:c2:~ ~:c3 ~:c4", location(0, 0), location(0, 12), location(0, 17));
        testSingleLineComment("~:c1 ~:c2:~ ~:c3:~ ~:c4", location(0, 0), location(0, 19));
        testSingleLineComment("~:c1 ~:c2:~ ~:c3:~ ~:c4:~", location(0, 0));
        testSingleLineComment("~:c1 ~:c2 ~:c3 ~:c4:~ :~ :~", location(0, 0));
        testSingleLineComment("~:c1~:c2~:c3~:c4:~:~:~", location(0, 0));
    }

    @Test
    @Order(35)
    public void testLexer_StringTokens_NestedComment_SingleLineCommentInMultiLineComment() {
        String sourceString = "~: ~:Single-line comment.:~\n:~";
        String[] lines = sourceString.split("\n");
        List<List<String>> statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:~:Single-line comment.:~\n:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:\n~:Single-line comment.:~ :~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:\n~:Single-line comment.:~:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~: ~:Nested multi-line\ncomment.:~ :~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:~:Nested multi-line\ncomment.:~:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:Nested ~:multi-\nline:~ comment.:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:Nested~:multi-\nline:~comment.:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:Multiple ~:nested ~:multi-\nline:~ comments:~ here.:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:Multiple~:nested~:multi-\nline:~comments:~here.:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.END);

        sourceString = "~:Multiple\n~:nested~:\nmulti-\nline\n:~comments:~\nhere.:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 6, tokenCounts(1, 1, 1, 1, 1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(4).get(0), lines[4].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(5).get(0), lines[5].length(), CommentTokenType.END);

        sourceString = "~: ~:c1 ~:c2:~ :~\n~:c3~:\nc4\n~:c5:~\n:~ c6:~\nc7:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 6, tokenCounts(1, 1, 1, 1, 1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(4).get(0), lines[4].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(5).get(0), lines[5].length(), CommentTokenType.END);

        sourceString = "~:~:c1~:c2:~:~\n~:c3~:\nc4\n~:c5:~\n:~c6:~\nc7:~";
        lines = sourceString.split("\n");
        statements = lexAsTokens(sourceString, 6, tokenCounts(1, 1, 1, 1, 1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(4).get(0), lines[4].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(5).get(0), lines[5].length(), CommentTokenType.END);

        // Complex example.
        sourceString = """
            ~:
            ~:Comment on its own line.:~
            var1:String << ``foo`` ~:End of line comment.:~
            ~:var2:Char << `a` ~:Commented out line.:~:~

            ~:This is a nested
            multi-line comment:~

            ~:
            This comment contains another ~:multi-line
            comment:~ within it.
            :~

            ~:Start of line comment.:~ var3:Int << 2

            ~:This comment ~:contains a nested comment:~ inside it.:~
            :~
            """;
        lines = sourceString.split("\n");
        int statementCount = lines.length;
        int[] tokenCounts = Collections.nCopies(statementCount, 1).stream().mapToInt(Integer::intValue).toArray();
        statements = lexAsTokens(sourceString, statementCount, tokenCounts);

        int finalIndex = statementCount - 1;
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        for (int i = 1; i < finalIndex; i++) {
            testComment(statements.get(i).get(0), lines[i].length(), CommentTokenType.MIDDLE);
        }
        testComment(statements.get(finalIndex).get(0), lines[finalIndex].length(), CommentTokenType.END);
    }

    @Test
    @Order(36)
    public void testLexer_StringTokens_NestedComment_SingleLineCommentInMultiLineComment_ErrorCases() {
        String sourceString = "~: ~:Single-line comment.\n:~";
        String[] lines = sourceString.split("\n");
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<String>> statements = lexAsTokens(sourceString, 2, errorReporter, 1, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);

        sourceString = "~: ~:Single-line comment.\n";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 1, errorReporter, 2, tokenCounts(1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 3), lines[0]);

        sourceString = "~:\n~:Single-line comment.:~";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 1, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);

        sourceString = "~:\n~:Single-line comment.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(1, 0), lines[1]);

        sourceString = "~:~:\nSingle-line comment.:~";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 1, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);

        sourceString = "~:~:\nSingle-line comment.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 2), lines[0]);

        sourceString = "~: ~:Nested multi-line\ncomment.:~";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 1, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);

        sourceString = "~:\n~:\nNested multi-line\ncomment.\n:~";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 5, errorReporter, 1, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(4).get(0), lines[4].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);

        sourceString = "~:Multiple ~:nested ~:multi-\nline:~ comments:~ here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 1, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);

        sourceString = "~:Multiple ~:nested ~:multi-\nline:~ comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 2, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 11), lines[0]);

        sourceString = "~:Multiple ~:nested ~:multi-\nline comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 2, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 11), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(2), location(0, 20), lines[0]);

        sourceString = "~:Multiple\n~:nested\n~:multi-\n~:line comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 4, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(1, 0), lines[1]);
        testCommentSyntaxError(syntaxErrors.get(2), location(2, 0), lines[2]);
        testCommentSyntaxError(syntaxErrors.get(3), location(3, 0), lines[3]);

        sourceString = "~:Multiple~:\nnested~:\nmulti-~:\nline comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 4, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 10), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(2), location(1, 6), lines[1]);
        testCommentSyntaxError(syntaxErrors.get(3), location(2, 6), lines[2]);

        sourceString = "~:Mult~:iple\nnes~:ted\nmul~:ti-\nline comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 4, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 6), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(2), location(1, 3), lines[1]);
        testCommentSyntaxError(syntaxErrors.get(3), location(2, 3), lines[2]);

        sourceString = "~:Multiple\n~:nested\n~:multi-\n~:line comments here:~.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(1, 0), lines[1]);
        testCommentSyntaxError(syntaxErrors.get(2), location(2, 0), lines[2]);

        sourceString = "~:Multiple~:\nnested~:\nmulti-~:\nline comments here:~.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 10), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(2), location(1, 6), lines[1]);

        sourceString = "~:Mult~:iple\nnes~:ted\nmul~:ti-\nline comments here:~.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 6), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(2), location(1, 3), lines[1]);

        sourceString = "~:Multiple\n~:nested\n~:mul:~ti-\n~:line comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(1, 0), lines[1]);
        testCommentSyntaxError(syntaxErrors.get(2), location(3, 0), lines[3]);

        sourceString = "~:Multiple~:\nnested~:\n:~multi-~:\nline comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 10), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(2), location(2, 8), lines[2]);

        sourceString = "~:Mult~:iple\nnes~:ted:~\nmul~:ti-\nline comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(0, 6), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(2), location(2, 3), lines[2]);

        sourceString = "~:Multiple\n~:nested:~\n~:multi-\n~:line comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(2, 0), lines[2]);
        testCommentSyntaxError(syntaxErrors.get(2), location(3, 0), lines[3]);

        sourceString = "~:Multiple~:\nnes:~ted~:\nmulti-~:\nline comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(1, 8), lines[1]);
        testCommentSyntaxError(syntaxErrors.get(2), location(2, 6), lines[2]);

        sourceString = "~:Mult~:iple\n:~nes~:ted\nmul~:ti-\nline comments here.";
        lines = sourceString.split("\n");
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, 4, errorReporter, 3, tokenCounts(1, 1));
        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        testComment(statements.get(1).get(0), lines[1].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(2).get(0), lines[2].length(), CommentTokenType.MIDDLE);
        testComment(statements.get(3).get(0), lines[3].length(), CommentTokenType.MIDDLE);
        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(1, 5), lines[1]);
        testCommentSyntaxError(syntaxErrors.get(2), location(2, 3), lines[2]);

        // Complex example.
        sourceString = """
            ~:
            ~:Comment on its own line.:~
            var1:String << ``foo`` ~:End of line comment with escaped closing token\\:~
            \\~:var2:Char << `a` \\~:Commented out line with escaped tokens.\\:~:~

            ~:This is a nested
            multi-line comment:~

            ~:
            This comment contains another ~:multi-line
            comment\\:~ within it but the closing token is missing.
            \\:~

            ~:Start of line comment.\\:~ var3:Int << 2

            ~:This comment ~:contains a nested comment\\:~ inside it.:~
            :~
            """;
        lines = sourceString.split("\n");
        int statementCount = lines.length;
        int[] tokenCounts = Collections.nCopies(statementCount, 1).stream().mapToInt(Integer::intValue).toArray();
        errorReporter = new SyntaxErrorReporter();
        statements = lexAsTokens(sourceString, statementCount, errorReporter, 4, tokenCounts);

        testComment(statements.get(0).get(0), lines[0].length(), CommentTokenType.START);
        for (int i = 1; i < statementCount; i++) {
            testComment(statements.get(i).get(0), lines[i].length(), CommentTokenType.MIDDLE);
        }

        syntaxErrors = errorReporter.getSyntaxErrors();
        testCommentSyntaxError(syntaxErrors.get(0), location(0, 0), lines[0]);
        testCommentSyntaxError(syntaxErrors.get(1), location(8, 0), lines[8]);
        testCommentSyntaxError(syntaxErrors.get(2), location(9, 30), lines[9]);
        testCommentSyntaxError(syntaxErrors.get(3), location(13, 0), lines[13]);
    }
}
