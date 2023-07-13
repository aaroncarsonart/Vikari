package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.DotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.RightParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.lexSingleStatement;
import static org.junit.jupiter.api.Assertions.fail;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_LineContinuations {

    /**
     * Ensure warnings are disabled for the LexerTestUtils methods outside of this test class.
     */
    @AfterAll
    public static void disableWarnings() {
        LexerTestUtils.setEnableWarnings(false);
    }

    private void lexVariableDeclaration(String sourceString, CoordinatePair... locations) {
        int expectedCrystalCount = 7;
        if (locations.length != expectedCrystalCount) {
            fail("Malformed test. Expected " + expectedCrystalCount + " locations for test method.");
        }

        for (int i = 0; i < 2; i++) {
            // Test expectations for warnings flag enabled or disabled should be entirely the same.
            LexerTestUtils.setEnableWarnings(i == 0);

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, expectedCrystalCount, errorReporter, 0);

            testCrystal(statement.get(0), ReferenceCrystal.class, "foo", locations[0]);
            testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", locations[1]);
            testCrystal(statement.get(2), TypeReferenceCrystal.class, "Int", locations[2]);
            testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", locations[3]);
            testCrystal(statement.get(4), IntegerCrystal.class, "5", locations[4]);
            testCrystal(statement.get(5), AddOperatorCrystal.class, "+", locations[5]);
            testCrystal(statement.get(6), DoubleCrystal.class, "2.0", locations[6]);

            assertNoWarnings(errorReporter);
        }
    }

    private void lexPrintStatement(String sourceString, CoordinatePair... locations) {
        int expectedCrystalCount = 10;
        if (locations.length != expectedCrystalCount) {
            fail("Malformed test. Expected " + expectedCrystalCount + " locations for test method.");
        }

        for (int i = 0; i < 2; i++) {
            // Test expectations for warnings flag enabled or disabled should be entirely the same.
            LexerTestUtils.setEnableWarnings(i == 0);

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, expectedCrystalCount, errorReporter, 0);

            testCrystal(statement.get(0), TypeLabelOperatorCrystal.class, ":", locations[0]);
            testCrystal(statement.get(1), StringLiteralCrystal.class, "``bar``", locations[1]);
            testCrystal(statement.get(2), TypeLabelOperatorCrystal.class, ":", locations[2]);
            testCrystal(statement.get(3), ReferenceCrystal.class, "baz", locations[3]);
            testCrystal(statement.get(4), DotOperatorCrystal.class, ".", locations[4]);
            testCrystal(statement.get(5), ReferenceCrystal.class, "buzz", locations[5]);
            testCrystal(statement.get(6), FunctionCallOperatorCrystal.class, "!", locations[6]);
            testCrystal(statement.get(7), LeftParenthesisCrystal.class, "(", locations[7]);
            testCrystal(statement.get(8), RightParenthesisCrystal.class, ")", locations[8]);
            testCrystal(statement.get(9), TypeLabelOperatorCrystal.class, ":", locations[9]);

            assertNoWarnings(errorReporter);
        }
    }

    @Test
    @Order(1)
    public void testLexer_Crystals_LineContinuation_VariableDeclaration_WithArithmeticInitializer() {
        lexVariableDeclaration("foo~\n:Int << 5 + 2.0", location(0, 0), location(1, 0), location(1, 1), location(1, 5),
                location(1, 8), location(1, 10), location(1, 12));

        lexVariableDeclaration("foo:~\nInt << 5 + 2.0", location(0, 0), location(0, 3), location(1, 0), location(1, 4),
                location(1, 7), location(1, 9), location(1, 11));

        lexVariableDeclaration("foo:Int~\n<< 5 + 2.0", location(0, 0), location(0, 3), location(0, 4), location(1, 0),
                location(1, 3), location(1, 5), location(1, 7));

        lexVariableDeclaration("foo:Int <<~\n5 + 2.0", location(0, 0), location(0, 3), location(0, 4), location(0, 8),
                location(1, 0), location(1, 2), location(1, 4));

        lexVariableDeclaration("foo:Int << 5~\n+ 2.0", location(0, 0), location(0, 3), location(0, 4), location(0, 8),
                location(0, 11), location(1, 0), location(1, 2));

        lexVariableDeclaration("foo:Int << 5 +~\n2.0", location(0, 0), location(0, 3), location(0, 4), location(0, 8),
                location(0, 11), location(0, 13), location(1, 0));
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_LineContinuation_VariableDeclaration_WithArithmeticInitializer_Multiple() {
        lexVariableDeclaration("foo:~\nInt <<~\n5 + 2.0", location(0, 0), location(0, 3), location(1, 0),
                location(1, 4), location(2, 0), location(2, 2), location(2, 4));

        lexVariableDeclaration("foo~\n:Int~\n<< 5~\n+ 2.0", location(0, 0), location(1, 0), location(1, 1),
                location(2, 0), location(2, 3), location(3, 0), location(3, 2));

        lexVariableDeclaration("foo~\n:~\nInt~\n<<~\n5~\n+~\n2.0", location(0, 0), location(1, 0), location(2, 0),
                location(3, 0), location(4, 0), location(5, 0), location(6, 0));
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_LineContinuation_PrintStatement() {
        lexPrintStatement(":``bar``:baz.buzz!():", location(0, 0), location(0, 1), location(0, 8), location(0, 9),
                location(0, 12), location(0, 13), location(0, 17), location(0, 18), location(0, 19), location(0, 20));

        lexPrintStatement(":~\n``bar``:baz.buzz!():", location(0, 0), location(1, 0), location(1, 7), location(1, 8),
                location(1, 11), location(1, 12), location(1, 16), location(1, 17), location(1, 18), location(1, 19));

        lexPrintStatement(":``bar``~\n:baz.buzz!():", location(0, 0), location(0, 1), location(1, 0), location(1, 1),
                location(1, 4), location(1, 5), location(1, 9), location(1, 10), location(1, 11), location(1, 12));

        lexPrintStatement(":``bar``:~\nbaz.buzz!():", location(0, 0), location(0, 1), location(0, 8), location(1, 0),
                location(1, 3), location(1, 4), location(1, 8), location(1, 9), location(1, 10), location(1, 11));

        lexPrintStatement(":``bar``:baz~\n.buzz!():", location(0, 0), location(0, 1), location(0, 8), location(0, 9),
                location(1, 0), location(1, 1), location(1, 5), location(1, 6), location(1, 7), location(1, 8));

        lexPrintStatement(":``bar``:baz.~\nbuzz!():", location(0, 0), location(0, 1), location(0, 8), location(0, 9),
                location(0, 12), location(1, 0), location(1, 4), location(1, 5), location(1, 6), location(1, 7));

        lexPrintStatement(":``bar``:baz.buzz~\n!():", location(0, 0), location(0, 1), location(0, 8), location(0, 9),
                location(0, 12), location(0, 13), location(1, 0), location(1, 1), location(1, 2), location(1, 3));

        lexPrintStatement(":``bar``:baz.buzz!~\n():", location(0, 0), location(0, 1), location(0, 8), location(0, 9),
                location(0, 12), location(0, 13), location(0, 17), location(1, 0), location(1, 1), location(1, 2));

        lexPrintStatement(":``bar``:baz.buzz!()~\n:", location(0, 0), location(0, 1), location(0, 8), location(0, 9),
                location(0, 12), location(0, 13), location(0, 17), location(0, 18), location(0, 19), location(1, 0));
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_LineContinuation_PrintStatement_Multiple() {
        lexPrintStatement(":``bar``~\n:baz.~\nbuzz!():", location(0, 0), location(0, 1), location(1, 0), location(1, 1),
                location(1, 4), location(2, 0), location(2, 4), location(2, 5), location(2, 6), location(2, 7));

        lexPrintStatement(":~\n``bar``:~\nbaz.buzz~\n!():", location(0, 0), location(1, 0), location(1, 7),
                location(2, 0), location(2, 3), location(2, 4), location(3, 0), location(3, 1), location(3, 2),
                location(3, 3));

        lexPrintStatement(":~\n``bar``~\n:~\nbaz~\n.~\nbuzz~\n!~\n()~\n:", location(0, 0), location(1, 0),
                location(2, 0), location(3, 0), location(4, 0), location(5, 0), location(6, 0), location(7, 0),
                location(7, 1), location(8, 0));
    }
}
