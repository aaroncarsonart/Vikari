package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.BlankLineCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RegionOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.ListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.RightParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.MultiLineStringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.SwordCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals {

    @Test
    @Order(1)
    public void testLexer_Crystals_BasicDefaultIdentifiers() {
        // Enclosures can't be individually lexed.
        Set<TokenType> enclosureTokenTypes = EnumSet.of(
                TokenType.COMMENT_PREFIX_CRYSTAL,
                TokenType.COMMENT_SUFFIX_CRYSTAL,
                TokenType.CAPTURE_QUOTATION,
                TokenType.BACKTICK);

        List<TokenType> tokenTypesToTest = TokenType.LEXER_TOKENS.stream()
                .filter(Predicate.not(enclosureTokenTypes::contains))
                .collect(Collectors.toCollection(ArrayList::new));

        for (TokenType tokenType : tokenTypesToTest) {
            String sourceString = tokenType.getIdentifier();
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
            testCrystal(statement.get(0), tokenType.getJavaType(), sourceString, location(0, 0));
        }
    }

    /**
     * Basic sanity test for initial handling of assignment for a default value of an untyped identifier.
     */
    @Test
    @Order(2)
    public void testLexer_Crystals_BasicAssignmentStatement() {
        String sourceString = "a << *";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 3);

        testCrystal(statement.get(0), ReferenceCrystal.class, "a", location(0, 0));
        testCrystal(statement.get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 2));
        testCrystal(statement.get(2), MultiplyOperatorCrystal.class, "*", location(0, 5));
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_CommentPrefix() {
        String sourceString = "~:";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexSingleStatement(sourceString, 1, errorReporter, 1);

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 0), "~:", "comment suffix");
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_SingleLineComment() {
        String sourceString = "~:This is a comment.:~";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);

        testCrystal(statement.get(0), BlankLineCrystal.class, "", location(0, 0));
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_TwoLineComment() {
        String sourceString = "~:This is a comment\n" +
                "across two lines.:~";

        List<List<AtonementCrystal>> statements = lex(sourceString, 2, crystalCounts(1, 1));

        testCrystal(statements.get(0).get(0), BlankLineCrystal.class, "", location(0, 0));
        testCrystal(statements.get(1).get(0), BlankLineCrystal.class, "", location(1, 0));
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_ThreeLineComment() {
        String sourceString = "~:This is a comment\n" +
                "across three lines\n" +
                "without indentation.:~";

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(1, 1, 1));

        testCrystal(statements.get(0).get(0), BlankLineCrystal.class, "", location(0, 0));
        testCrystal(statements.get(1).get(0), BlankLineCrystal.class, "", location(1, 0));
        testCrystal(statements.get(2).get(0), BlankLineCrystal.class, "", location(2, 0));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_CommentCrystalsOnSeparateLines() {
        String sourceString = "~:\n" +
                "This is a multi-line comment\n" +
                "with the comment crystals on\n" +
                "separate lines from its contents.\n" +
                ":~";

        List<List<AtonementCrystal>> statements = lex(sourceString, 5, crystalCounts(1, 1, 1, 1, 1));

        testCrystal(statements.get(0).get(0), BlankLineCrystal.class, "", location(0, 0));
        testCrystal(statements.get(1).get(0), BlankLineCrystal.class, "", location(1, 0));
        testCrystal(statements.get(2).get(0), BlankLineCrystal.class, "", location(2, 0));
        testCrystal(statements.get(3).get(0), BlankLineCrystal.class, "", location(3, 0));
        testCrystal(statements.get(4).get(0), BlankLineCrystal.class, "", location(4, 0));
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_SingleLineStringLiteral() {
        String sourceString = "``This is a string literal.``";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), StringLiteralCrystal.class, sourceString, location(0, 0));
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_TwoLineStringLiteral() {
        String sourceString = "``This is a string literal\n" +
                "across two lines.``";

        List<List<AtonementCrystal>> statements = lex(sourceString, 2, crystalCounts(1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a string literal", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "across two lines.``", location(1, 0));

        testLinkage(crystal1, crystal2);
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_ThreeLineStringLiteral() {
        String sourceString = "``This is a string literal\n" +
                "across three lines\n" +
                "without indentation.``";

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a string literal", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "across three lines", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "without indentation.``", location(2, 0));

        testLinkage(crystal1, crystal2, crystal3);
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_MultiLineStringLiteral_WithOtherCrystals_BeforeAndAfter() {
        String sourceString = "foo << bar!(``This is a string literal\n" +
                "across two lines.``|baz)";

        List<List<AtonementCrystal>> statements = lex(sourceString, 2, crystalCounts(6, 4));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 0));
        testCrystal(statements.get(0).get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 4));
        testCrystal(statements.get(0).get(2), ReferenceCrystal.class, "bar", location(0, 7));
        testCrystal(statements.get(0).get(3), FunctionCallOperatorCrystal.class, "!", location(0, 10));
        testCrystal(statements.get(0).get(4), LeftParenthesisCrystal.class, "(", location(0, 11));

        MultiLineStringLiteralCrystal string1 = testMultiLineStringLiteral(statements.get(0).get(5), "``This is a string literal", location(0, 12));
        MultiLineStringLiteralCrystal string2 = testMultiLineStringLiteral(statements.get(1).get(0), "across two lines.``", location(1, 0));

        testCrystal(statements.get(1).get(1), ListElementSeparatorCrystal.class, "|", location(1, 19));
        testCrystal(statements.get(1).get(2), ReferenceCrystal.class, "baz", location(1, 20));
        testCrystal(statements.get(1).get(3), RightParenthesisCrystal.class, ")", location(1, 23));

        testLinkage(string1, string2);
    }

    @Test
    @Order(12)
    public void testLexer_Crystals_CaptureQuotations_EnclosingCode_SingleLine() {
        String sourceString = "``a << *``";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0),StringLiteralCrystal.class, sourceString, location(0, 0));
    }

    @Test
    @Order(13)
    public void testLexer_Crystals_CaptureQuotations_EnclosingCode_OnSeparateLines_SingleLine() {
        String sourceString = "string << ``\n" +
                              "a << *\n" +
                              "`` + foo";

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(3, 1, 3));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "string", location(0, 0));
        testCrystal(statements.get(0).get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 7));

        MultiLineStringLiteralCrystal string1 = testMultiLineStringLiteral(statements.get(0).get(2), "``", location(0, 10));
        MultiLineStringLiteralCrystal string2 = testMultiLineStringLiteral(statements.get(1).get(0), "a << *", location(1, 0));
        MultiLineStringLiteralCrystal string3 = testMultiLineStringLiteral(statements.get(2).get(0), "``", location(2, 0));

        testCrystal(statements.get(2).get(1), AddOperatorCrystal.class, "+", location(2, 3));
        testCrystal(statements.get(2).get(2), ReferenceCrystal.class, "foo", location(2, 5));

        testLinkage(string1, string2, string3);
    }

    @Test
    @Order(14)
    public void testLexer_Crystals_CaptureQuotations_EnclosingCode_OnSeparateLines_MultipleLines() {
        String sourceString = "``\n" +
                "foo << 2 + [3 * -7]\n" +
                "bar << foo - [5 / 9.0]\n" +
                ":foo + bar\n" +
                "``";

        List<List<AtonementCrystal>> statements = lex(sourceString, 5, crystalCounts(1, 1, 1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "foo << 2 + [3 * -7]", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "bar << foo - [5 / 9.0]", location(2, 0));
        MultiLineStringLiteralCrystal crystal4 = testMultiLineStringLiteral(statements.get(3).get(0), ":foo + bar", location(3, 0));
        MultiLineStringLiteralCrystal crystal5 = testMultiLineStringLiteral(statements.get(4).get(0), "``", location(4, 0));

        testLinkage(crystal1, crystal2, crystal3, crystal4, crystal5);
    }

    @Test
    @Order(15)
    public void testLexer_Crystals_ErrorCase_SingularBacktick() {
        String sourceString = "`";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexSingleStatement(sourceString, 1, errorReporter, 1);

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "backtick");
    }

    @Test
    @Order(16)
    public void testLexer_Crystals_TrueLiteral() {
        String sourceString = "true";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), BooleanLiteralCrystal.class, "true", location(0, 0));
    }

    @Test
    @Order(17)
    public void testLexer_Crystals_FalseLiteral() {
        String sourceString = "false";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), BooleanLiteralCrystal.class, "false", location(0, 0));
    }

    @Test
    @Order(18)
    public void testLexer_Crystals_OverloadedTokenTypes() {
        List<TokenType> overloadedTypesToExpect = List.of(
                TokenType.MODULUS, TokenType.MULTIPLY, TokenType.SUBTRACT, TokenType.DELETE, TokenType.ADD);

        for (TokenType tokenType : overloadedTypesToExpect) {
            String sourceString = tokenType.getIdentifier();
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
            testCrystal(statement.get(0), tokenType.getJavaType(), sourceString, location(0, 0));
        }
    }

    @Test
    @Order(19)
    public void testLexer_Crystals_SwordOfLengthOne() {
        String sourceString = "_";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), SwordCrystal.class, "_", location(0, 0));

        SwordCrystal swordCrystal = (SwordCrystal) statement.get(0);
        assertEquals(1, swordCrystal.getLength(), "Unexpected sword length.");
    }

    @Test
    @Order(20)
    public void testLexer_Crystals_SwordOfLengthTwo() {
        String sourceString = "__";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), SwordCrystal.class, "__", location(0, 0));

        SwordCrystal swordCrystal = (SwordCrystal) statement.get(0);
        assertEquals(2, swordCrystal.getLength(), "Unexpected sword length.");
    }

    @Test
    @Order(21)
    public void testLexer_Crystals_SwordOfLengthThree() {
        String sourceString = "___";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), SwordCrystal.class, "___", location(0, 0));

        SwordCrystal swordCrystal = (SwordCrystal) statement.get(0);
        assertEquals(3, swordCrystal.getLength(), "Unexpected sword length.");
    }

    @Test
    @Order(22)
    public void testLexer_Crystals_TypeReferences_TypeLabel() {
        String sourceString = "int:Integer";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 3);

        testCrystal(statement.get(0), ReferenceCrystal.class, "int", location(0, 0));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 3));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, 4));
    }

    @Test
    @Order(23)
    public void testLexer_Crystals_TypeReferences_StaticFunctionCall() {
        String sourceString = "Math::abs!(-1)";
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 7);

        testCrystal(statement.get(0), TypeReferenceCrystal.class, "Math", location(0, 0));
        testCrystal(statement.get(1), RegionOperatorCrystal.class, "::", location(0, 4));
        testCrystal(statement.get(2), ReferenceCrystal.class, "abs", location(0, 6));
        testCrystal(statement.get(3), FunctionCallOperatorCrystal.class, "!", location(0, 9));
        testCrystal(statement.get(4), LeftParenthesisCrystal.class, "(", location(0, 10));
        testCrystal(statement.get(5), IntegerCrystal.class, "-1", location(0, 12));
        testCrystal(statement.get(6), RightParenthesisCrystal.class, ")", location(0, 13));
    }
}
