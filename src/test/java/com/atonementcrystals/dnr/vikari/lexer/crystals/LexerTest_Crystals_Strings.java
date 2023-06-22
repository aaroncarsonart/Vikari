package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.MultiLineStringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

public class LexerTest_Crystals_Strings {

    @Test
    @Order(1)
    public void testLexer_Crystals_Strings_QuotedCaptureQuotation_AtStartOfString() {
        String sourceString = "``\\``This string has a quoted capture quotation.``";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), StringLiteralCrystal.class, sourceString, location(0, 0));
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_Strings_QuotedCaptureQuotation_InMiddleOfString() {
        String sourceString = "``This string has a quoted capture quotation: \\`` and then it ends here.``";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), StringLiteralCrystal.class, sourceString, location(0, 0));
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_Strings_QuotedBacktick_AtEndOfString() {
        String sourceString = "``This string has a quoted backtick: \\```";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        testCrystal(statement.get(0), StringLiteralCrystal.class, sourceString, location(0, 0));
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_AtStartOfFirstToken() {
        String sourceString = """
            ``\\`` This is a multi-line string which
            contains a quoted capture quotation.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 2, tokenCounts(1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``\\`` This is a multi-line string which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "contains a quoted capture quotation.``", location(1, 0));

        testLinkage(crystal1, crystal2);
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_InMiddleOfFirstToken() {
        String sourceString = """
            ``This is a multi-line string \\`` which
            contains a quoted capture quotation.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 2, tokenCounts(1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string \\`` which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "contains a quoted capture quotation.``", location(1, 0));

        testLinkage(crystal1, crystal2);
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_AtEndOfFirstToken() {
        String sourceString = """
            ``This is a multi-line string \\``
            which contains a quoted capture quotation.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 2, tokenCounts(1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string \\``", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "which contains a quoted capture quotation.``", location(1, 0));

        testLinkage(crystal1, crystal2);
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_AtStartOfMiddleToken() {
        String sourceString = """
            ``This is a multi-line string which
            \\`` contains a quoted capture quotation
            at the start of the middle token.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, tokenCounts(1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "\\`` contains a quoted capture quotation", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "at the start of the middle token.``", location(2, 0));

        testLinkage(crystal1, crystal2, crystal3);
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_InMiddleOfMiddleToken() {
        String sourceString = """
            ``This is a multi-line string which
            contains a quoted \\`` capture quotation
            in the middle of the middle token.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, tokenCounts(1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "contains a quoted \\`` capture quotation", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "in the middle of the middle token.``", location(2, 0));

        testLinkage(crystal1, crystal2, crystal3);
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_AtEndOfMiddleToken() {
        String sourceString = """
            ``This is a multi-line string which
            contains a quoted capture quotation \\``
            at the end of the middle token.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, tokenCounts(1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "contains a quoted capture quotation \\``", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "at the end of the middle token.``", location(2, 0));

        testLinkage(crystal1, crystal2, crystal3);
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_AtStartOfEndToken() {
        String sourceString = """
            ``This is a multi-line string which
            contains a quoted capture quotation
            \\`` at the start of the end token.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, tokenCounts(1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "contains a quoted capture quotation", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "\\`` at the start of the end token.``", location(2, 0));

        testLinkage(crystal1, crystal2, crystal3);
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_InMiddleOfEndToken() {
        String sourceString = """
            ``This is a multi-line string which
            contains a quoted capture quotation
            in the middle \\`` of the end token.``
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, tokenCounts(1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "contains a quoted capture quotation", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "in the middle \\`` of the end token.``", location(2, 0));

        testLinkage(crystal1, crystal2, crystal3);
    }

    @Test
    @Order(12)
    public void testLexer_Crystals_MultiLineStrings_QuotedBacktick_AtEndOfEndToken() {
        String sourceString = """
            ``This is a multi-line string which
            contains a quoted capture quotation
            at the end of the end token.\\```
            """;

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, tokenCounts(1, 1, 1));

        MultiLineStringLiteralCrystal crystal1 = testMultiLineStringLiteral(statements.get(0).get(0), "``This is a multi-line string which", location(0, 0));
        MultiLineStringLiteralCrystal crystal2 = testMultiLineStringLiteral(statements.get(1).get(0), "contains a quoted capture quotation", location(1, 0));
        MultiLineStringLiteralCrystal crystal3 = testMultiLineStringLiteral(statements.get(2).get(0), "at the end of the end token.\\```", location(2, 0));

        testLinkage(crystal1, crystal2, crystal3);
    }

}
