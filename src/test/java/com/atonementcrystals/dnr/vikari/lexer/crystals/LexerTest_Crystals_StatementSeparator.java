package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.DotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.InstanceFieldAccessOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.LeftSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.RightSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.ListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.RightParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testCrystal;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_StatementSeparator {

    private class TestPair {
        private String sourceString;
        private CoordinatePair offset;

        public TestPair(String sourceString, CoordinatePair offset) {
            this.sourceString = sourceString;
            this.offset = offset;
        }
    }

    private TestPair testPair(String sourceString, CoordinatePair offset) {
        return new TestPair(sourceString, offset);
    }

    @Test
    @Order(1)
    public void testStatementSeparator_WithBlankStatements() {
        // 1. Only statement separators.
        lex(",", 0);
        lex(",,", 0);
        lex(",,,", 0);
        lex(",,,,", 0);
        lex(",,,,,", 0);

        // 2. With newlines before.
        lex("\n,", 0);
        lex("\n,\n,", 0);
        lex("\n,\n,\n,", 0);
        lex("\n,\n,\n,\n,", 0);
        lex("\n,\n,\n,\n,\n,", 0);

        // 3. With newlines after.
        lex(",\n", 0);
        lex(",\n,\n", 0);
        lex(",\n,\n,\n", 0);
        lex(",\n,\n,\n,\n", 0);
        lex(",\n,\n,\n,\n,\n", 0);

        // 4. With newlines before and after.
        lex("\n,\n", 0);
        lex("\n,\n,\n", 0);
        lex("\n,\n,\n,\n", 0);
        lex("\n,\n,\n,\n,\n", 0);
        lex("\n,\n,\n,\n,\n,\n", 0);

        // 5. With multiple newlines.
        lex("\n\n\n,\n\n\n", 0);
        lex("\n\n\n,\n\n\n,\n\n\n", 0);
        lex("\n\n\n,\n\n\n,\n\n\n,\n\n\n", 0);
        lex("\n\n\n,\n\n\n,\n\n\n,\n\n\n,\n\n\n", 0);
        lex("\n\n\n,\n\n\n,\n\n\n,\n\n\n,\n\n\n,\n\n\n", 0);
    }

    @Test
    @Order(2)
    public void testStatementSeparator_SingleStatement_TerminatedByStatementSeparator() {
        List<AtonementCrystal> statement;
        List<String> sourceStrings;

        // ---------------------------
        // Single statement separator.
        // ---------------------------

        // Variable declaration.
        sourceStrings = List.of(
                "foo:Integer << 2,",
                "foo:Integer << 2,,",
                "foo:Integer << 2,,,",
                "foo:Integer << 2,,,,",
                "foo:Integer << 2\n,",
                "foo:Integer << 2\n,,",
                "foo:Integer << 2\n,,,",
                "foo:Integer << 2\n,,,,",
                "foo:Integer << 2,\n",
                "foo:Integer << 2,,\n",
                "foo:Integer << 2,,,\n",
                "foo:Integer << 2,,,,\n");

        for (String sourceString : sourceStrings) {
            statement = lexSingleStatement(sourceString, 5);

            testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
            testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 3));
            testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, 4));
            testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, 12));
            testCrystal(statement.get(4), IntegerCrystal.class, "2", location(0, 15));
        }

        // Arithmetic Expression.
        sourceStrings = List.of(
                "[a + 7] / 3.0F,",
                "[a + 7] / 3.0F,,",
                "[a + 7] / 3.0F,,,",
                "[a + 7] / 3.0F,,,,",
                "[a + 7] / 3.0F\n,",
                "[a + 7] / 3.0F\n,,",
                "[a + 7] / 3.0F\n,,,",
                "[a + 7] / 3.0F\n,,,,",
                "[a + 7] / 3.0F,\n",
                "[a + 7] / 3.0F,,\n",
                "[a + 7] / 3.0F,,,\n",
                "[a + 7] / 3.0F,,,,\n");

        for (String sourceString : sourceStrings) {
            statement = lexSingleStatement(sourceString, 7);

            testCrystal(statement.get(0), LeftSquareBracketCrystal.class, "[", location(0, 0));
            testCrystal(statement.get(1), ReferenceCrystal.class, "a", location(0, 1));
            testCrystal(statement.get(2), AddOperatorCrystal.class, "+", location(0, 3));
            testCrystal(statement.get(3), IntegerCrystal.class, "7", location(0, 5));
            testCrystal(statement.get(4), RightSquareBracketCrystal.class, "]", location(0, 6));
            testCrystal(statement.get(5), LeftDivideOperatorCrystal.class, "/", location(0, 8));
            testCrystal(statement.get(6), FloatCrystal.class, "3.0F", location(0, 10));
        }

        // Print statement with function call.
        sourceStrings = List.of(
                ":``bar``:baz.buzz!(`a`|5|@b):,",
                ":``bar``:baz.buzz!(`a`|5|@b):,,",
                ":``bar``:baz.buzz!(`a`|5|@b):,,,",
                ":``bar``:baz.buzz!(`a`|5|@b):,,,,",
                ":``bar``:baz.buzz!(`a`|5|@b):\n,",
                ":``bar``:baz.buzz!(`a`|5|@b):\n,,",
                ":``bar``:baz.buzz!(`a`|5|@b):\n,,,",
                ":``bar``:baz.buzz!(`a`|5|@b):\n,,,,",
                ":``bar``:baz.buzz!(`a`|5|@b):,\n",
                ":``bar``:baz.buzz!(`a`|5|@b):,,\n",
                ":``bar``:baz.buzz!(`a`|5|@b):,,,\n",
                ":``bar``:baz.buzz!(`a`|5|@b):,,,,\n");

        for (String sourceString : sourceStrings) {
            statement = lexSingleStatement(sourceString, 16);

            testCrystal(statement.get(0), TypeLabelOperatorCrystal.class, ":", location(0, 0));
            testCrystal(statement.get(1), StringLiteralCrystal.class, "``bar``", location(0, 1));
            testCrystal(statement.get(2), TypeLabelOperatorCrystal.class, ":", location(0, 8));
            testCrystal(statement.get(3), ReferenceCrystal.class, "baz", location(0, 9));
            testCrystal(statement.get(4), DotOperatorCrystal.class, ".", location(0, 12));
            testCrystal(statement.get(5), ReferenceCrystal.class, "buzz", location(0, 13));
            testCrystal(statement.get(6), FunctionCallOperatorCrystal.class, "!", location(0, 17));
            testCrystal(statement.get(7), LeftParenthesisCrystal.class, "(", location(0, 18));
            testCrystal(statement.get(8), ReferenceCrystal.class, "`a`", location(0, 19));
            testCrystal(statement.get(9), ListElementSeparatorCrystal.class, "|", location(0, 22));
            testCrystal(statement.get(10), IntegerCrystal.class, "5", location(0, 23));
            testCrystal(statement.get(11), ListElementSeparatorCrystal.class, "|", location(0, 24));
            testCrystal(statement.get(12), InstanceFieldAccessOperatorCrystal.class, "@", location(0, 25));
            testCrystal(statement.get(13), ReferenceCrystal.class, "b", location(0, 26));
            testCrystal(statement.get(14), RightParenthesisCrystal.class, ")", location(0, 27));
            testCrystal(statement.get(15), TypeLabelOperatorCrystal.class, ":", location(0, 28));
        }
    }

    @Test
    @Order(3)
    public void testStatementSeparator_SingleStatement_PrefixedByStatementSeparator() {
        List<AtonementCrystal> statement;
        List<TestPair> testPairs;

        // ---------------------------
        // Single statement separator.
        // ---------------------------

        // Variable declaration.
        testPairs = List.of(
                testPair(",foo:Integer << 2", location(0, 1)),
                testPair(",,foo:Integer << 2", location(0, 2)),
                testPair(",,,foo:Integer << 2", location(0, 3)),
                testPair(",,,,foo:Integer << 2", location(0, 4)),
                testPair("\n,foo:Integer << 2,", location(1, 1)),
                testPair("\n,,foo:Integer << 2", location(1, 2)),
                testPair("\n,,,foo:Integer << 2", location(1, 3)),
                testPair("\n,,,,foo:Integer << 2", location(1, 4)),
                testPair(",\nfoo:Integer << 2", location(1, 0)),
                testPair(",,\nfoo:Integer << 2", location(1, 0)),
                testPair(",,,\nfoo:Integer << 2", location(1, 0)),
                testPair(",,,,\nfoo:Integer << 2", location(1, 0)));

        for (TestPair testPair : testPairs) {
            statement = lexSingleStatement(testPair.sourceString, 5);

            int row = testPair.offset.getRow();
            int col = testPair.offset.getColumn();

            testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(row, col));
            testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(row, col + 3));
            testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(row, col + 4));
            testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(row, col + 12));
            testCrystal(statement.get(4), IntegerCrystal.class, "2", location(row, col + 15));
        }

        // Arithmetic Expression.
        testPairs = List.of(
                testPair(",[a + 7] / 3.0F", location(0, 1)),
                testPair(",,[a + 7] / 3.0F", location(0, 2)),
                testPair(",,,[a + 7] / 3.0F", location(0, 3)),
                testPair(",,,,[a + 7] / 3.0F", location(0, 4)),
                testPair("\n,[a + 7] / 3.0F,", location(1, 1)),
                testPair("\n,,[a + 7] / 3.0F", location(1, 2)),
                testPair("\n,,,[a + 7] / 3.0F", location(1, 3)),
                testPair("\n,,,,[a + 7] / 3.0F", location(1, 4)),
                testPair(",\n[a + 7] / 3.0F", location(1, 0)),
                testPair(",,\n[a + 7] / 3.0F", location(1, 0)),
                testPair(",,,\n[a + 7] / 3.0F", location(1, 0)),
                testPair(",,,,\n[a + 7] / 3.0F", location(1, 0)));

        for (TestPair testPair : testPairs) {
            statement = lexSingleStatement(testPair.sourceString, 7);

            int row = testPair.offset.getRow();
            int col = testPair.offset.getColumn();

            testCrystal(statement.get(0), LeftSquareBracketCrystal.class, "[", location(row, col));
            testCrystal(statement.get(1), ReferenceCrystal.class, "a", location(row, col + 1));
            testCrystal(statement.get(2), AddOperatorCrystal.class, "+", location(row, col + 3));
            testCrystal(statement.get(3), IntegerCrystal.class, "7", location(row, col + 5));
            testCrystal(statement.get(4), RightSquareBracketCrystal.class, "]", location(row, col + 6));
            testCrystal(statement.get(5), LeftDivideOperatorCrystal.class, "/", location(row, col + 8));
            testCrystal(statement.get(6), FloatCrystal.class, "3.0F", location(row, col + 10));
        }

        // Print statement with function call.
        testPairs = List.of(
                testPair(",:``bar``:baz.buzz!(`a`|5|@b):", location(0, 1)),
                testPair(",,:``bar``:baz.buzz!(`a`|5|@b):", location(0, 2)),
                testPair(",,,:``bar``:baz.buzz!(`a`|5|@b):", location(0, 3)),
                testPair(",,,,:``bar``:baz.buzz!(`a`|5|@b):", location(0, 4)),
                testPair("\n,:``bar``:baz.buzz!(`a`|5|@b):,", location(1, 1)),
                testPair("\n,,:``bar``:baz.buzz!(`a`|5|@b):", location(1, 2)),
                testPair("\n,,,:``bar``:baz.buzz!(`a`|5|@b):", location(1, 3)),
                testPair("\n,,,,:``bar``:baz.buzz!(`a`|5|@b):", location(1, 4)),
                testPair(",\n:``bar``:baz.buzz!(`a`|5|@b):", location(1, 0)),
                testPair(",,\n:``bar``:baz.buzz!(`a`|5|@b):", location(1, 0)),
                testPair(",,,\n:``bar``:baz.buzz!(`a`|5|@b):", location(1, 0)),
                testPair(",,,,\n:``bar``:baz.buzz!(`a`|5|@b):", location(1, 0)));

        for (TestPair testPair : testPairs) {
            statement = lexSingleStatement(testPair.sourceString, 16);

            int row = testPair.offset.getRow();
            int col = testPair.offset.getColumn();

            testCrystal(statement.get(0), TypeLabelOperatorCrystal.class, ":", location(row, col));
            testCrystal(statement.get(1), StringLiteralCrystal.class, "``bar``", location(row, col + 1));
            testCrystal(statement.get(2), TypeLabelOperatorCrystal.class, ":", location(row, col + 8));
            testCrystal(statement.get(3), ReferenceCrystal.class, "baz", location(row, col + 9));
            testCrystal(statement.get(4), DotOperatorCrystal.class, ".", location(row, col + 12));
            testCrystal(statement.get(5), ReferenceCrystal.class, "buzz", location(row, col + 13));
            testCrystal(statement.get(6), FunctionCallOperatorCrystal.class, "!", location(row, col + 17));
            testCrystal(statement.get(7), LeftParenthesisCrystal.class, "(", location(row, col + 18));
            testCrystal(statement.get(8), ReferenceCrystal.class, "`a`", location(row, col + 19));
            testCrystal(statement.get(9), ListElementSeparatorCrystal.class, "|", location(row, col + 22));
            testCrystal(statement.get(10), IntegerCrystal.class, "5", location(row, col + 23));
            testCrystal(statement.get(11), ListElementSeparatorCrystal.class, "|", location(row, col + 24));
            testCrystal(statement.get(12), InstanceFieldAccessOperatorCrystal.class, "@", location(row, col + 25));
            testCrystal(statement.get(13), ReferenceCrystal.class, "b", location(row, col + 26));
            testCrystal(statement.get(14), RightParenthesisCrystal.class, ")", location(row, col + 27));
            testCrystal(statement.get(15), TypeLabelOperatorCrystal.class, ":", location(row, col + 28));
        }
    }

    private void testThreeStatements(String sourceString, CoordinatePair... locations) {
        assertEquals(3, locations.length, "Malformed test: unexpected number of locations.");

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(1, 1, 1));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", locations[0]);
        testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", locations[1]);
        testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", locations[2]);
    }

    @Test
    @Order(4)
    public void testStatementSeparator_MultipleStatements() {
        // Single commas without newlines.
        testThreeStatements("foo,bar,baz", location(0, 0), location(0, 4), location(0, 8));
        testThreeStatements("foo, bar, baz", location(0, 0), location(0, 5), location(0, 10));
        testThreeStatements("foo ,bar ,baz", location(0, 0), location(0, 5), location(0, 10));
        testThreeStatements("foo , bar , baz", location(0, 0), location(0, 6), location(0, 12));

        // Single commas with newlines.
        testThreeStatements("foo\n,bar\n,baz", location(0, 0), location(1, 1), location(2, 1));
        testThreeStatements("foo,\nbar,\nbaz", location(0, 0), location(1, 0), location(2, 0));
        testThreeStatements("foo\n,\nbar\n,\nbaz", location(0, 0), location(2, 0), location(4, 0));

        // Two commas without newlines.
        testThreeStatements("foo,,bar,,baz", location(0, 0), location(0, 5), location(0, 10));
        testThreeStatements("foo,, bar,, baz", location(0, 0), location(0, 6), location(0, 12));
        testThreeStatements("foo ,,bar ,,baz", location(0, 0), location(0, 6), location(0, 12));
        testThreeStatements("foo ,, bar ,, baz", location(0, 0), location(0, 7), location(0, 14));

        // Two commas with newlines.
        testThreeStatements("foo\n,,bar\n,,baz", location(0, 0), location(1, 2), location(2, 2));
        testThreeStatements("foo,,\nbar,,\nbaz", location(0, 0), location(1, 0), location(2, 0));
        testThreeStatements("foo\n,,\nbar\n,,\nbaz", location(0, 0), location(2, 0), location(4, 0));

        // Multiple commas without newlines.
        testThreeStatements("foo,,,,bar,,,,baz", location(0, 0), location(0, 7), location(0, 14));
        testThreeStatements("foo,,,, bar,,,, baz", location(0, 0), location(0, 8), location(0, 16));
        testThreeStatements("foo ,,,,bar ,,,,baz", location(0, 0), location(0, 8), location(0, 16));
        testThreeStatements("foo ,,,, bar ,,,, baz", location(0, 0), location(0, 9), location(0, 18));

        // Multiple commas with newlines.
        testThreeStatements("foo\n,,,,bar\n,,,,baz", location(0, 0), location(1, 4), location(2, 4));
        testThreeStatements("foo,,,,\nbar,,,,\nbaz", location(0, 0), location(1, 0), location(2, 0));
        testThreeStatements("foo\n,,,,\nbar\n,,,,\nbaz", location(0, 0), location(2, 0), location(4, 0));
    }

    private void testThreeLongStatements(String sourceString, CoordinatePair... offsets) {
        assertEquals(3, offsets.length, "Malformed test: unexpected number of offsets.");

        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(5, 7, 16));

        // 1. Variable declaration.
        List<AtonementCrystal> statement = statements.get(0);
        int row = offsets[0].getRow();
        int col = offsets[0].getColumn();

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(row, col));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(row, col + 3));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(row, col + 4));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(row, col + 12));
        testCrystal(statement.get(4), IntegerCrystal.class, "2", location(row, col + 15));

        // 2. Expression statement.
        statement = statements.get(1);
        row = offsets[1].getRow();
        col = offsets[1].getColumn();

        testCrystal(statement.get(0), LeftSquareBracketCrystal.class, "[", location(row, col));
        testCrystal(statement.get(1), ReferenceCrystal.class, "a", location(row, col + 1));
        testCrystal(statement.get(2), AddOperatorCrystal.class, "+", location(row, col + 3));
        testCrystal(statement.get(3), IntegerCrystal.class, "7", location(row, col + 5));
        testCrystal(statement.get(4), RightSquareBracketCrystal.class, "]", location(row, col + 6));
        testCrystal(statement.get(5), LeftDivideOperatorCrystal.class, "/", location(row, col + 8));
        testCrystal(statement.get(6), FloatCrystal.class, "3.0F", location(row, col + 10));

        // 3. Print statement with function call.
        statement = statements.get(2);
        row = offsets[2].getRow();
        col = offsets[2].getColumn();

        testCrystal(statement.get(0), TypeLabelOperatorCrystal.class, ":", location(row, col));
        testCrystal(statement.get(1), StringLiteralCrystal.class, "``bar``", location(row, col + 1));
        testCrystal(statement.get(2), TypeLabelOperatorCrystal.class, ":", location(row, col + 8));
        testCrystal(statement.get(3), ReferenceCrystal.class, "baz", location(row, col + 9));
        testCrystal(statement.get(4), DotOperatorCrystal.class, ".", location(row, col + 12));
        testCrystal(statement.get(5), ReferenceCrystal.class, "buzz", location(row, col + 13));
        testCrystal(statement.get(6), FunctionCallOperatorCrystal.class, "!", location(row, col + 17));
        testCrystal(statement.get(7), LeftParenthesisCrystal.class, "(", location(row, col + 18));
        testCrystal(statement.get(8), ReferenceCrystal.class, "`a`", location(row, col + 19));
        testCrystal(statement.get(9), ListElementSeparatorCrystal.class, "|", location(row, col + 22));
        testCrystal(statement.get(10), IntegerCrystal.class, "5", location(row, col + 23));
        testCrystal(statement.get(11), ListElementSeparatorCrystal.class, "|", location(row, col + 24));
        testCrystal(statement.get(12), InstanceFieldAccessOperatorCrystal.class, "@", location(row, col + 25));
        testCrystal(statement.get(13), ReferenceCrystal.class, "b", location(row, col + 26));
        testCrystal(statement.get(14), RightParenthesisCrystal.class, ")", location(row, col + 27));
        testCrystal(statement.get(15), TypeLabelOperatorCrystal.class, ":", location(row, col + 28));
    }

    @Test
    @Order(5)
    public void testStatementSeparator_MultipleLongStatements() {
        String variableDeclaration ="foo:Integer << 2";
        String expressionStatement = "[a + 7] / 3.0F";
        String printStatement = ":``bar``:baz.buzz!(`a`|5|@b):";

        // 1. One statement separator.
        String sourceString = variableDeclaration + "," + expressionStatement + "," + printStatement + ",";
        testThreeLongStatements(sourceString, location(0, 0), location(0, 17), location(0, 32));

        sourceString = variableDeclaration + "\n," + expressionStatement + "\n," + printStatement + "\n,";
        testThreeLongStatements(sourceString, location(0, 0), location(1, 1), location(2, 1));

        sourceString = variableDeclaration + ",\n" + expressionStatement + ",\n" + printStatement + ",\n";
        testThreeLongStatements(sourceString, location(0, 0), location(1, 0), location(2, 0));

        sourceString = variableDeclaration + "\n,\n" + expressionStatement + "\n,\n" + printStatement + "\n,\n";
        testThreeLongStatements(sourceString, location(0, 0), location(2, 0), location(4, 0));

        // 2. Two statement separators.
        sourceString = variableDeclaration + ",," + expressionStatement + ",," + printStatement + ",,";
        testThreeLongStatements(sourceString, location(0, 0), location(0, 18), location(0, 34));

        sourceString = variableDeclaration + "\n,," + expressionStatement + "\n,," + printStatement + "\n,,";
        testThreeLongStatements(sourceString, location(0, 0), location(1, 2), location(2, 2));

        sourceString = variableDeclaration + ",,\n" + expressionStatement + ",,\n" + printStatement + ",,\n";
        testThreeLongStatements(sourceString, location(0, 0), location(1, 0), location(2, 0));

        sourceString = variableDeclaration + "\n,,\n" + expressionStatement + "\n,,\n" + printStatement + "\n,,\n";
        testThreeLongStatements(sourceString, location(0, 0), location(2, 0), location(4, 0));

        // 3. Four statement separators.
        sourceString = variableDeclaration + ",,,," + expressionStatement + ",,,," + printStatement + ",,,,";
        testThreeLongStatements(sourceString, location(0, 0), location(0, 20), location(0, 38));

        sourceString = variableDeclaration + "\n,,,," + expressionStatement + "\n,,,," + printStatement + "\n,,,,";
        testThreeLongStatements(sourceString, location(0, 0), location(1, 4), location(2, 4));

        sourceString = variableDeclaration + ",,,,\n" + expressionStatement + ",,,,\n" + printStatement + ",,,,\n";
        testThreeLongStatements(sourceString, location(0, 0), location(1, 0), location(2, 0));

        sourceString = variableDeclaration + "\n,,,,\n" + expressionStatement + "\n,,,,\n" + printStatement + "\n,,,,\n";
        testThreeLongStatements(sourceString, location(0, 0), location(2, 0), location(4, 0));
    }
}
