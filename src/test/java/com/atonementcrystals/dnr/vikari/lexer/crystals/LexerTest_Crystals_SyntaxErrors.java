package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.MultiLineStringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.quotation.BacktickQuotationCrystal;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

/**
 * The syntax errors for these cases have already been tested in the individual
 * LexerTest classes. So the following tests only validate the correct crystal
 * types are used for when the results are passed to the parser.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_SyntaxErrors {

    @Test
    @Order(1)
    public void testLexer_Crystals_SyntaxErrors_CommentPrefix() {
        String sourceString = "~:";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lex(sourceString, 0, syntaxErrorReporter, 1);
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_SyntaxErrors_SingularBacktick() {
        String sourceString = "`";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), BacktickQuotationCrystal.class, "`", location(0, 0));
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_SingleBacktickQuotation_IdentifierContainingNewline() {
        String sourceString = "`foo\n`:Integer << 0";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 2, syntaxErrorReporter, 2, crystalCounts(1, 1));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "`foo", location(0, 0));
        testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "`:Integer << 0", location(1, 0));
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_SingleBacktickQuotation_IdentifierMissingClosingBacktickQuote() {
        String sourceString = "foo:Integer << `bar";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 3));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, 4));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, 12));
        testCrystal(statement.get(4), ReferenceCrystal.class, "`bar", location(0, 15));
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_SingleBacktickQuotation_ContainingTabsShouldFail() {
        String sourceString = "`foo\tbar`:Integer << 2";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "`foo\tbar`", location(0, 0));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 9));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, 10));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, 18));
        testCrystal(statement.get(4), IntegerCrystal.class, "2", location(0, 21));
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_SingleBacktickQuotation_OnlyWhitespaceCharactersShouldFail() {
        // -----------------------
        // single space (is valid)
        // -----------------------
        String sourceString = "space:Character << ` `";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5);

        testCrystal(statement.get(0), ReferenceCrystal.class, "space", location(0, 0));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 5));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Character", location(0, 6));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, 16));
        testCrystal(statement.get(4), ReferenceCrystal.class, "` `", location(0, 19));

        // ---------------
        // multiple spaces
        // ---------------
        sourceString = "`   ` << AtonementCrystal!";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatement(sourceString, 4, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "`   `", location(0, 0));
        testCrystal(statement.get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 6));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "AtonementCrystal", location(0, 9));
        testCrystal(statement.get(3), FunctionCallOperatorCrystal.class, "!", location(0, 25));

        // ---------------------
        // single tab (is valid)
        // ---------------------
        sourceString = "foo << `\t`";

        syntaxErrorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatement(sourceString, 3, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
        testCrystal(statement.get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 4));
        testCrystal(statement.get(2), ReferenceCrystal.class, "`\t`", location(0, 7));

        // -------------
        // multiple tabs
        // -------------
        sourceString = "`\t\t` << AtonementCrystal!";

        syntaxErrorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatement(sourceString, 4, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "`\t\t`", location(0, 0));
        testCrystal(statement.get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 5));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "AtonementCrystal", location(0, 8));
        testCrystal(statement.get(3), FunctionCallOperatorCrystal.class, "!", location(0, 24));

        // ----------------------
        // mix of spaces and tabs
        // ----------------------
        sourceString = "` \t  \t\t   ` << AtonementCrystal!";

        syntaxErrorReporter = new SyntaxErrorReporter();
        statement = lexSingleStatement(sourceString, 4, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "` \t  \t\t   `", location(0, 0));
        testCrystal(statement.get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 12));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "AtonementCrystal", location(0, 15));
        testCrystal(statement.get(3), FunctionCallOperatorCrystal.class, "!", location(0, 31));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_CommentEnclosure_ErrorHandlingForUnclosedComment_SingleLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lex(sourceString, 0, syntaxErrorReporter, 1);
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_CommentEnclosure_ErrorHandlingForUnclosedComment_MultiLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].\n" +
                              "However, I forgot to close this comment!";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lex(sourceString, 0, syntaxErrorReporter, 1);
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_CaptureQuotations_ErrorHandlingForUnclosedString_SingleLine() {
        String sourceString = "``This is a malformed string literal.";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1, errorReporter, 1);

        MultiLineStringLiteralCrystal crystal = testMultiLineStringLiteral(statement.get(0), sourceString, location(0, 0));
        testLinkage(crystal);
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_CaptureQuotations_ErrorHandlingForUnclosedString_MultiLine() {
        String sourceString = "``This is a malformed string literal \n" +
                              "because it has no ending capture quotation!";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 2, errorReporter, 1, crystalCounts(1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a malformed string literal " , location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "because it has no ending capture quotation!" , location(1, 0));

        testLinkage(crystal1, crystal2);
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_SyntaxErrorCombos_multipleBacktickQuotations_andCaptureQuotation() {
        String sourceString = "a << `foo\n" +
                              "`z\tz` << a * 2\n" +
                              "bar:String << `  `\n" +
                              ":``baz``:``buzz:";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 4, errorReporter, 4, crystalCounts(3, 5, 5, 4));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "a", location(0, 0));
        testCrystal(statements.get(0).get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 2));
        testCrystal(statements.get(0).get(2), ReferenceCrystal.class, "`foo", location(0, 5));

        testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "`z\tz`", location(1, 0));
        testCrystal(statements.get(1).get(1), LeftAssignmentOperatorCrystal.class, "<<", location(1, 6));
        testCrystal(statements.get(1).get(2), ReferenceCrystal.class, "a", location(1, 9));
        testCrystal(statements.get(1).get(3), MultiplyOperatorCrystal.class, "*", location(1, 11));
        testCrystal(statements.get(1).get(4), IntegerCrystal.class, "2", location(1, 13));

        testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "bar", location(2, 0));
        testCrystal(statements.get(2).get(1), TypeLabelOperatorCrystal.class, ":", location(2, 3));
        testCrystal(statements.get(2).get(2), TypeReferenceCrystal.class, "String", location(2, 4));
        testCrystal(statements.get(2).get(3), LeftAssignmentOperatorCrystal.class, "<<", location(2, 11));
        testCrystal(statements.get(2).get(4), ReferenceCrystal.class, "`  `", location(2, 14));

        testCrystal(statements.get(3).get(0), TypeLabelOperatorCrystal.class, ":", location(3, 0));
        testCrystal(statements.get(3).get(1), StringLiteralCrystal.class, "``baz``", location(3, 1));
        testCrystal(statements.get(3).get(2), TypeLabelOperatorCrystal.class, ":", location(3, 8));
        MultiLineStringLiteralCrystal string = testMultiLineStringLiteral(statements.get(3).get(3), "``buzz:", location(3, 9));

        testLinkage(string);
    }

    @Test
    @Order(12)
    public void testLexer_Crystals_SyntaxErrorCombos_multipleErrorsOnSameLine_andCommentSuffix() {
        String sourceString = "`z\tz` << `foo\n" +
                              "~:Unclosed comment.";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 3, errorReporter, 3);

        testCrystal(statement.get(0), ReferenceCrystal.class, "`z\tz`", location(0, 0));
        testCrystal(statement.get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 6));
        testCrystal(statement.get(2), ReferenceCrystal.class, "`foo", location(0, 9));
    }

    /**
     * TODO: Change test design and expectations once indented code is fully supported.
     */
    @Test
    @Order(13)
    public void testLexer_Crystals_SyntaxErrors_tabIndentedCode() {
        String sourceString = "\t\t`z` << `foo\n" +
                              "\t\t~:Unclosed comment.";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 3, errorReporter, 4);

        testCrystal(statement.get(0), ReferenceCrystal.class, "`z`", location(0, 2));
        testCrystal(statement.get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 6));
        testCrystal(statement.get(2), ReferenceCrystal.class, "`foo", location(0, 9));
    }

    // ----------------------------------------------------------------------------
    // Test cases for SyntaxErrors actually generated by convertTokensToCrystals().
    // ----------------------------------------------------------------------------

    @Test
    @Order(14)
    public void testLexer_Crystals_SyntaxErrors_invalidIdentifier_LeadingDigit() {
        String sourceString = "3a";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 2);

        testCrystal(statement.get(0), IntegerCrystal.class, "3", location(0, 0));
        testCrystal(statement.get(1), ReferenceCrystal.class, "a", location(0, 1));
    }

    /**
     * Ensure that missing a backtick quotation generates only 1 syntax error, not 2.
     */
    @Test
    @Order(15)
    public void testLexer_Crystals_SyntaxErrors_invalidIdentifier_UnclosedBacktick() {
        String sourceString = "`b";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "`b", location(0, 0));
    }

    /**
     * Ensure a single backtick only reports 1 error, not two.
     */
    @Test
    @Order(16)
    public void testLexer_Crystals_SyntaxErrors_SingleBacktick() {
        String sourceString = "`";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1, syntaxErrorReporter, 1);

        testCrystal(statement.get(0), BacktickQuotationCrystal.class, "`", location(0, 0));
    }
}
