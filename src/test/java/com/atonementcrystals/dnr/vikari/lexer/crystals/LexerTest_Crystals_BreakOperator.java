package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow.BreakOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.LeftSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.RightSquareBracketCrystal;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testCrystal;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_BreakOperator {

    @Test
    @Order(1)
    public void testLexer_Crystals_BreakOperator() {
        String sourceString = "vv 2";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 2);

        testCrystal(statement.get(0), BreakOperatorCrystal.class, "vv", location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "2", location(0, 3));
    }

    /**
     * Ensure the break operator does not break tokens of the form /\w?vv\d+\w?/.
     */
    @Test
    @Order(2)
    public void testLexer_Crystals_BreakOperator_WithoutSpaces() {
        List<String> sourceStrings = List.of("vv2", "vv2a", "avv", "avv2", "avv2a", "a2vv2a");
        for (String sourceString : sourceStrings) {
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
            testCrystal(statement.get(0), ReferenceCrystal.class, sourceString, location(0, 0));
        }
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_BreakOperator_ArithmeticExpression() {
        String sourceString = "vv 2 + 5";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 4);

        testCrystal(statement.get(0), BreakOperatorCrystal.class, "vv", location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "2", location(0, 3));
        testCrystal(statement.get(2), AddOperatorCrystal.class, "+", location(0, 5));
        testCrystal(statement.get(3), IntegerCrystal.class, "5", location(0, 7));
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_BreakOperator_WithBrackets() {
        String sourceString = "vv [2]";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 4);

        testCrystal(statement.get(0), BreakOperatorCrystal.class, "vv", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 3));
        testCrystal(statement.get(2), IntegerCrystal.class, "2", location(0, 4));
        testCrystal(statement.get(3), RightSquareBracketCrystal.class, "]", location(0, 5));
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_BreakOperator_ArithmeticExpressionWithBrackets() {
        String sourceString = "vv [2 + 5]";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 6);

        testCrystal(statement.get(0), BreakOperatorCrystal.class, "vv", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 3));
        testCrystal(statement.get(2), IntegerCrystal.class, "2", location(0, 4));
        testCrystal(statement.get(3), AddOperatorCrystal.class, "+", location(0, 6));
        testCrystal(statement.get(4), IntegerCrystal.class, "5", location(0, 8));
        testCrystal(statement.get(5), RightSquareBracketCrystal.class, "]", location(0, 9));
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_BreakOperator_WithBrackets_NoSpaces() {
        String sourceString = "vv[2]";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 4);

        testCrystal(statement.get(0), BreakOperatorCrystal.class, "vv", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 2));
        testCrystal(statement.get(2), IntegerCrystal.class, "2", location(0, 3));
        testCrystal(statement.get(3), RightSquareBracketCrystal.class, "]", location(0, 4));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_BreakOperator_ArithmeticExpressionWithBrackets_NoSpaces() {
        String sourceString = "vv[2+5]";

        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 6);

        testCrystal(statement.get(0), BreakOperatorCrystal.class, "vv", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 2));
        testCrystal(statement.get(2), IntegerCrystal.class, "2", location(0, 3));
        testCrystal(statement.get(3), AddOperatorCrystal.class, "+", location(0, 4));
        testCrystal(statement.get(4), IntegerCrystal.class, "5", location(0, 5));
        testCrystal(statement.get(5), RightSquareBracketCrystal.class, "]", location(0, 6));
    }
}
