package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.control.flow.ConditionalBranchCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.error.ThrowCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow.ReturnOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RegionOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RegionSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.LeftSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RightSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.ListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RightParenthesisCrystal;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testCrystal;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_ThrowStatement {

    @Test
    @Order(1)
    public void testLexer_Crystals_ThrowStatement_Basic() {
        List<AtonementCrystal> statement = lexSingleStatement("--[foo]", 4);

        testCrystal(statement.get(0), ThrowCrystal.class, "--", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 2));
        testCrystal(statement.get(2), ReferenceCrystal.class, "foo", location(0, 3));
        testCrystal(statement.get(3), RightSquareBracketCrystal.class, "]", location(0, 6));
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_ThrowStatement_AfterStatementSeparator() {
        List<List<AtonementCrystal>> statements = lex(":foo:, --[bar]", 2, crystalCounts(3, 4));

        testCrystal(statements.get(0).get(0), TypeLabelOperatorCrystal.class, ":", location(0, 0));
        testCrystal(statements.get(0).get(1), ReferenceCrystal.class, "foo", location(0, 1));
        testCrystal(statements.get(0).get(2), TypeLabelOperatorCrystal.class, ":", location(0, 4));

        testCrystal(statements.get(1).get(0), ThrowCrystal.class, "--", location(0, 7));
        testCrystal(statements.get(1).get(1), LeftSquareBracketCrystal.class, "[", location(0, 9));
        testCrystal(statements.get(1).get(2), ReferenceCrystal.class, "bar", location(0, 10));
        testCrystal(statements.get(1).get(3), RightSquareBracketCrystal.class, "]", location(0, 13));
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_ThrowStatement_AfterRegionSeparator() {
        List<AtonementCrystal> statement = lexSingleStatement("?? [foo] :: ^^bar; --[baz]", 12);

        testCrystal(statement.get(0), ConditionalBranchCrystal.class, "??", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 3));
        testCrystal(statement.get(2), ReferenceCrystal.class, "foo", location(0, 4));
        testCrystal(statement.get(3), RightSquareBracketCrystal.class, "]", location(0, 7));
        testCrystal(statement.get(4), RegionOperatorCrystal.class, "::", location(0, 9));

        testCrystal(statement.get(5), ReturnOperatorCrystal.class, "^^", location(0, 12));
        testCrystal(statement.get(6), ReferenceCrystal.class, "bar", location(0, 14));
        testCrystal(statement.get(7), RegionSeparatorCrystal.class, ";", location(0, 17));

        testCrystal(statement.get(8), ThrowCrystal.class, "--", location(0, 19));
        testCrystal(statement.get(9), LeftSquareBracketCrystal.class, "[", location(0, 21));
        testCrystal(statement.get(10), ReferenceCrystal.class, "baz", location(0, 22));
        testCrystal(statement.get(11), RightSquareBracketCrystal.class, "]", location(0, 25));
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_ThrowStatement_AfterRegionOperator() {
        List<AtonementCrystal> statement = lexSingleStatement("?? [foo] :: --[bar]", 9);

        testCrystal(statement.get(0), ConditionalBranchCrystal.class, "??", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 3));
        testCrystal(statement.get(2), ReferenceCrystal.class, "foo", location(0, 4));
        testCrystal(statement.get(3), RightSquareBracketCrystal.class, "]", location(0, 7));
        testCrystal(statement.get(4), RegionOperatorCrystal.class, "::", location(0, 9));

        testCrystal(statement.get(5), ThrowCrystal.class, "--", location(0, 12));
        testCrystal(statement.get(6), LeftSquareBracketCrystal.class, "[", location(0, 14));
        testCrystal(statement.get(7), ReferenceCrystal.class, "bar", location(0, 15));
        testCrystal(statement.get(8), RightSquareBracketCrystal.class, "]", location(0, 18));
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_MinimizedSubtractNegativeNumber() {
        List<AtonementCrystal> statement = lexSingleStatement("5--2", 3);

        testCrystal(statement.get(0), IntegerCrystal.class, "5", location(0, 0));
        testCrystal(statement.get(1), SubtractOperatorCrystal.class, "-", location(0, 1));
        testCrystal(statement.get(2), IntegerCrystal.class, "-2", location(0, 3));
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_MinimizedSubtractNegatedVariable() {
        List<AtonementCrystal> statement = lexSingleStatement("5--foo", 4);

        testCrystal(statement.get(0), IntegerCrystal.class, "5", location(0, 0));
        testCrystal(statement.get(1), SubtractOperatorCrystal.class, "-", location(0, 1));
        testCrystal(statement.get(2), SubtractOperatorCrystal.class, "-", location(0, 2));
        testCrystal(statement.get(3), ReferenceCrystal.class, "foo", location(0, 3));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_SequentialNegations_WithinGroupingExpressions() {
        List<AtonementCrystal> statement = lexSingleStatement("[--bar]+[--5]", 10);

        testCrystal(statement.get(0), LeftSquareBracketCrystal.class, "[", location(0, 0));
        testCrystal(statement.get(1), SubtractOperatorCrystal.class, "-", location(0, 1));
        testCrystal(statement.get(2), SubtractOperatorCrystal.class, "-", location(0, 2));
        testCrystal(statement.get(3), ReferenceCrystal.class, "bar", location(0, 3));
        testCrystal(statement.get(4), RightSquareBracketCrystal.class, "]", location(0, 6));

        testCrystal(statement.get(5), AddOperatorCrystal.class, "+", location(0, 7));

        testCrystal(statement.get(6), LeftSquareBracketCrystal.class, "[", location(0, 8));
        testCrystal(statement.get(7), SubtractOperatorCrystal.class, "-", location(0, 9));
        testCrystal(statement.get(8), IntegerCrystal.class, "-5", location(0, 11));
        testCrystal(statement.get(9), RightSquareBracketCrystal.class, "]", location(0, 12));
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_SequentialNegations_WithinListLiteral() {
        List<AtonementCrystal> statement = lexSingleStatement("(--bar|--5)", 8);

        testCrystal(statement.get(0), LeftParenthesisCrystal.class, "(", location(0, 0));
        testCrystal(statement.get(1), SubtractOperatorCrystal.class, "-", location(0, 1));
        testCrystal(statement.get(2), SubtractOperatorCrystal.class, "-", location(0, 2));
        testCrystal(statement.get(3), ReferenceCrystal.class, "bar", location(0, 3));
        testCrystal(statement.get(4), ListElementSeparatorCrystal.class, "|", location(0, 6));

        testCrystal(statement.get(5), SubtractOperatorCrystal.class, "-", location(0, 7));
        testCrystal(statement.get(6), IntegerCrystal.class, "-5", location(0, 9));
        testCrystal(statement.get(7), RightParenthesisCrystal.class, ")", location(0, 10));
    }
}
