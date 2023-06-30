package com.atonementcrystals.dnr.vikari.lexer.tokens;

import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testSyntaxError;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

/**
 * Ensure multiple error types can all occur together, and be reported accurately
 * all for the same source string.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_StringTokens_SyntaxErrors {

    @Test
    @Order(1)
    public void testLexer_StringTokens_SyntaxErrorCombos_multipleBacktickQuotations_andCaptureQuotation() {
        String sourceString = "a << `foo\n" +
                              "`z\tz` << a * 2\n" +
                              "bar:String << `  `\n" +
                              ":``baz``:``buzz:";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<String>> statements = lexAsTokens(sourceString, 4, errorReporter, 4, tokenCounts(5, 9, 7, 4));

        testToken(statements.get(0).get(0), "a");
        testToken(statements.get(0).get(1), " ");
        testToken(statements.get(0).get(2), "<<");
        testToken(statements.get(0).get(3), " ");
        testToken(statements.get(0).get(4), "`foo");

        testToken(statements.get(1).get(0), "`z\tz`");
        testToken(statements.get(1).get(1), " ");
        testToken(statements.get(1).get(2), "<<");
        testToken(statements.get(1).get(3), " ");
        testToken(statements.get(1).get(4), "a");
        testToken(statements.get(1).get(5), " ");
        testToken(statements.get(1).get(6), "*");
        testToken(statements.get(1).get(7), " ");
        testToken(statements.get(1).get(8), "2");

        testToken(statements.get(2).get(0), "bar");
        testToken(statements.get(2).get(1), ":");
        testToken(statements.get(2).get(2), "String");
        testToken(statements.get(2).get(3), " ");
        testToken(statements.get(2).get(4), "<<");
        testToken(statements.get(2).get(5), " ");
        testToken(statements.get(2).get(6), "`  `");

        testToken(statements.get(3).get(0), ":");
        testToken(statements.get(3).get(1), "``baz``");
        testToken(statements.get(3).get(2), ":");
        testToken(statements.get(3).get(3), "``buzz:");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();

        testSyntaxError(syntaxErrors.get(0), location(0, 5), "a << `foo", "Missing closing backtick quotation");
        testSyntaxError(syntaxErrors.get(1), location(1, 1), "`z\tz` << a * 2", "Backtick-quoted identifiers cannot contain tabs");
        testSyntaxError(syntaxErrors.get(2), location(2, 15), "bar:String << `  `", "Backtick-quoted identifiers cannot contain only whitespace");
        testSyntaxError(syntaxErrors.get(3), location(3, 9), ":``baz``:``buzz:", "Missing closing capture quotation");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_SyntaxErrorCombos_multipleErrorsOnSameLine_andCommentSuffix() {
        String sourceString = "`z\tz` << `foo\n" +
                              "~:Unclosed comment.";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<String>> statements = lexAsTokens(sourceString, 2, errorReporter, 3, tokenCounts(5, 1));

        testToken(statements.get(0).get(0), "`z\tz`");
        testToken(statements.get(0).get(1), " ");
        testToken(statements.get(0).get(2), "<<");
        testToken(statements.get(0).get(3), " ");
        testToken(statements.get(0).get(4), "`foo");

        testComment(statements.get(1).get(0), "~:Unclosed comment.".length(), CommentTokenType.START);

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();

        testSyntaxError(syntaxErrors.get(0), location(0, 1), "`z\tz` << `foo", "Backtick-quoted identifiers cannot contain tabs");
        testSyntaxError(syntaxErrors.get(1), location(0, 9), "`z\tz` << `foo", "Missing closing backtick quotation");
        testSyntaxError(syntaxErrors.get(2), location(1, 0), "~:Unclosed comment.", "Missing comment suffix token");
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_SyntaxErrors_tabIndentedCode() {
        String sourceString = "\t\t`zz` << `foo\n" +
                              "\t\t~:Unclosed comment.";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<String>> statements = lexAsTokens(sourceString, 2, errorReporter, 2, tokenCounts(6, 2));

        testToken(statements.get(0).get(0), "\t\t");
        testToken(statements.get(0).get(1), "`zz`");
        testToken(statements.get(0).get(2), " ");
        testToken(statements.get(0).get(3), "<<");
        testToken(statements.get(0).get(4), " ");
        testToken(statements.get(0).get(5), "`foo");

        testToken(statements.get(1).get(0), "\t\t");
        testComment(statements.get(1).get(1), "~:Unclosed comment.".length(), CommentTokenType.START);

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();

        testSyntaxError(syntaxErrors.get(0), location(0, 10), "\t\t`zz` << `foo", "Missing closing backtick quotation");
        testSyntaxError(syntaxErrors.get(1), location(1, 2), "\t\t~:Unclosed comment.", "Missing comment suffix token");
    }
}
