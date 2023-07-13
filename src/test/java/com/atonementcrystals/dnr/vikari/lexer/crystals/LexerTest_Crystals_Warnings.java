package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.DotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.DeleteOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.LeftSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.RightSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.RightParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_Warnings {

    private static final String ONLY_LINE_CONTINUATIONS = "Statement contains only line continuations.";
    private static final String START_OF_STATEMENT = "Unnecessary line continuation at start of statement.";
    private static final String END_OF_STATEMENT = "Unnecessary line continuation at end of statement.";

    /**
     * Ensure warnings are disabled for the LexerTestUtils methods outside of this test class.
     */
    @AfterAll
    public static void disableWarnings() {
        LexerTestUtils.setEnableWarnings(false);
    }

    private void testOnlyLineContinuations(String sourceString) {
        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, errorReporter, 0);

            if (warningsEnabled) {
                assertWarnings(errorReporter, 1);
                List<VikariError> warnings = errorReporter.getCompilationWarnings();

                String[] lines = sourceString.split("\n");
                String expectedLine = lines[0];
                testWarning(warnings.get(0), location(0, 0), expectedLine, ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }

    @Test
    @Order(1)
    public void testLexer_Crystals_Warnings_OnlyLineContinuations() {
        testOnlyLineContinuations("~");
        testOnlyLineContinuations("~\n~");
        testOnlyLineContinuations("~\n~\n~");
        testOnlyLineContinuations("~\n~\n~\n~");
        testOnlyLineContinuations("~\n~\n~\n~\n~");
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_Warnings_LineContinuation_AtStartOfStatement() {
        String sourceString = "~\na + 5";

        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, 3, errorReporter, 0);

            testCrystal(statement.get(0), ReferenceCrystal.class, "a", location(1, 0));
            testCrystal(statement.get(1), AddOperatorCrystal.class, "+", location(1, 2));
            testCrystal(statement.get(2), IntegerCrystal.class, "5", location(1, 4));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 1);
                List<VikariError> warnings = errorReporter.getCompilationWarnings();

                String[] lines = sourceString.split("\n");
                String expectedLine = lines[0];
                testWarning(warnings.get(0), location(0, 0), expectedLine, START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }
    @Test
    @Order(3)
    public void testLexer_Crystals_Warnings_LineContinuation_AtEndOfStatement() {
        String sourceString = "foo.bar!()~";

        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, 6, errorReporter, 0);

            testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
            testCrystal(statement.get(1), DotOperatorCrystal.class, ".", location(0, 3));
            testCrystal(statement.get(2), ReferenceCrystal.class, "bar", location(0, 4));
            testCrystal(statement.get(3), FunctionCallOperatorCrystal.class, "!", location(0, 7));
            testCrystal(statement.get(4), LeftParenthesisCrystal.class, "(", location(0, 8));
            testCrystal(statement.get(5), RightParenthesisCrystal.class, ")", location(0, 9));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 1);
                List<VikariError> warnings = errorReporter.getCompilationWarnings();

                String[] lines = sourceString.split("\n");
                String expectedLine = lines[0];
                testWarning(warnings.get(0), location(0, 10), expectedLine, END_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_Warnings_LineContinuation_AtStartAndEndOfStatement() {
        String sourceString = "~\n:``foo``:[7.0F / 3B]:~";

        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, 9, errorReporter, 0);

            testCrystal(statement.get(0), TypeLabelOperatorCrystal.class, ":", location(1, 0));
            testCrystal(statement.get(1), StringLiteralCrystal.class, "``foo``", location(1, 1));
            testCrystal(statement.get(2), TypeLabelOperatorCrystal.class, ":", location(1, 8));
            testCrystal(statement.get(3), LeftSquareBracketCrystal.class, "[", location(1, 9));
            testCrystal(statement.get(4), FloatCrystal.class, "7.0F", location(1, 10));
            testCrystal(statement.get(5), LeftDivideOperatorCrystal.class, "/", location(1, 15));
            testCrystal(statement.get(6), BigIntegerCrystal.class, "3B", location(1, 17));
            testCrystal(statement.get(7), RightSquareBracketCrystal.class, "]", location(1, 19));
            testCrystal(statement.get(8), TypeLabelOperatorCrystal.class, ":", location(1, 20));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 2);
                List<VikariError> warnings = errorReporter.getCompilationWarnings();

                String[] lines = sourceString.split("\n");
                testWarning(warnings.get(0), location(0, 0), lines[0], "Unnecessary line continuation at start of statement.");
                testWarning(warnings.get(1), location(1, 21), lines[1], "Unnecessary line continuation at end of statement.");
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_Warnings_LineContinuation_ThreeStatements_AtStartOfStatement() {
        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            // Terminated by newlines.
            String sourceString = "~\nfoo\n\n~\nbar\n\n~\nbaz";

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(4, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(7, 0));

            List<VikariError> warnings;

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                 warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(3, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(6, 0), "~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas.
            sourceString = "~\nfoo,~\nbar,~\nbaz,";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(2, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(3, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(1, 4), "foo,~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(2, 4), "bar,~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines before.
            sourceString = "~\nfoo\n,~\nbar\n,~\nbaz\n,";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(3, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(5, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(2, 1), ",~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(4, 1), ",~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines after.
            sourceString = "~\nfoo,\n~\nbar,\n~\nbaz,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(3, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(5, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(2, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(4, 0), "~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines before and after.
            sourceString = "~\nfoo\n,\n~\nbar\n,\n~\nbaz\n,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(4, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(7, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(3, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(6, 0), "~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_Warnings_LineContinuation_ThreeStatements_AtEndOfStatement() {
        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            // Terminated by newlines.
            String sourceString = "foo~\n\nbar~\n\nbaz~\n";

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(2, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(4, 0));

            List<VikariError> warnings;

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 3), "foo~", END_OF_STATEMENT);
                testWarning(warnings.get(1), location(2, 3), "bar~", END_OF_STATEMENT);
                testWarning(warnings.get(2), location(4, 3), "baz~", END_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas case is invalid.
            sourceString = "foo~,bar~,baz~,";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(2, 2, 2));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 0));
            testCrystal(statements.get(0).get(1), DeleteOperatorCrystal.class, "~", location(0, 3));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(0, 5));
            testCrystal(statements.get(1).get(1), DeleteOperatorCrystal.class, "~", location(0, 8));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(0, 10));
            testCrystal(statements.get(2).get(1), DeleteOperatorCrystal.class, "~", location(0, 13));

            assertNoWarnings(errorReporter);

            // Terminated by commas with newlines before.
            sourceString = "foo~\n,bar~\n,baz~\n,";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(1, 1));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(2, 1));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 3), "foo~", END_OF_STATEMENT);
                testWarning(warnings.get(1), location(1, 4), ",bar~", END_OF_STATEMENT);
                testWarning(warnings.get(2), location(2, 4), ",baz~", END_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas with newlines after case is invalid.
            sourceString = "foo~,\nbar~,\nbaz~,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(2, 2, 2));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 0));
            testCrystal(statements.get(0).get(1), DeleteOperatorCrystal.class, "~", location(0, 3));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(1, 0));
            testCrystal(statements.get(1).get(1), DeleteOperatorCrystal.class, "~", location(1, 3));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(2, 0));
            testCrystal(statements.get(2).get(1), DeleteOperatorCrystal.class, "~", location(2, 3));

            assertNoWarnings(errorReporter);

            // Terminated by commas with newlines before and after.
            sourceString = "foo~\n,\nbar~\n,\nbaz~\n,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(2, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(4, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 3), "foo~", END_OF_STATEMENT);
                testWarning(warnings.get(1), location(2, 3), "bar~", END_OF_STATEMENT);
                testWarning(warnings.get(2), location(4, 3), "baz~", END_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_Warnings_LineContinuation_ThreeStatements_AtStartAndEndOfStatement() {
        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            // Terminated by newlines.
            String sourceString = "~\nfoo~\n\n~\nbar~\n\n~\nbaz~\n";

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(4, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(7, 0));

            List<VikariError> warnings;

            if (warningsEnabled) {
                assertWarnings(errorReporter, 6);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(1, 3), "foo~", END_OF_STATEMENT);
                testWarning(warnings.get(2), location(3, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(3), location(4, 3), "bar~", END_OF_STATEMENT);
                testWarning(warnings.get(4), location(6, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(5), location(7, 3), "baz~", END_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas case is invalid.
            sourceString = "~\nfoo~,~\nbar~,~\nbaz~,";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(2, 2, 2));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(0).get(1), DeleteOperatorCrystal.class, "~", location(1, 3));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(2, 0));
            testCrystal(statements.get(1).get(1), DeleteOperatorCrystal.class, "~", location(2, 3));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(3, 0));
            testCrystal(statements.get(2).get(1), DeleteOperatorCrystal.class, "~", location(3, 3));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(1, 5), "foo~,~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(2, 5), "bar~,~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines before.
            sourceString = "~\nfoo~\n,~\nbar~\n,~\nbaz~\n,";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(3, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(5, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 6);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(1, 3), "foo~", END_OF_STATEMENT);
                testWarning(warnings.get(2), location(2, 1), ",~", START_OF_STATEMENT);
                testWarning(warnings.get(3), location(3, 3), "bar~", END_OF_STATEMENT);
                testWarning(warnings.get(4), location(4, 1), ",~", START_OF_STATEMENT);
                testWarning(warnings.get(5), location(5, 3), "baz~", END_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas with newlines after case is invalid.
            sourceString = "~\nfoo~,\n~\nbar~,\n~\nbaz~,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(2, 2, 2));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(0).get(1), DeleteOperatorCrystal.class, "~", location(1, 3));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(3, 0));
            testCrystal(statements.get(1).get(1), DeleteOperatorCrystal.class, "~", location(3, 3));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(5, 0));
            testCrystal(statements.get(2).get(1), DeleteOperatorCrystal.class, "~", location(5, 3));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(2, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(4, 0), "~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines before and after.
            sourceString = "~\nfoo~\n,\n~\nbar~\n,\n~\nbaz~\n,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(1, 0));
            testCrystal(statements.get(1).get(0), ReferenceCrystal.class, "bar", location(4, 0));
            testCrystal(statements.get(2).get(0), ReferenceCrystal.class, "baz", location(7, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 6);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(1, 3), "foo~", END_OF_STATEMENT);
                testWarning(warnings.get(2), location(3, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(3), location(4, 3), "bar~", END_OF_STATEMENT);
                testWarning(warnings.get(4), location(6, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(5), location(7, 3), "baz~", END_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_Warnings_LineContinuation_ThreeStatements_OnlyLineContinuations() {
        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            // Terminated by newlines.
            String sourceString = "~\n\n~\n\n~\n\n";

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, errorReporter, 0);

            List<VikariError> warnings;

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(1), location(2, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(2), location(4, 0), "~", ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas case is invalid.
            sourceString = "~,~,~";

            errorReporter = new SyntaxErrorReporter();
            List<List<AtonementCrystal>> statements = lex(sourceString, 2, errorReporter, 0);

            testCrystal(statements.get(0).get(0), DeleteOperatorCrystal.class, "~", location(0, 0));
            testCrystal(statements.get(1).get(0), DeleteOperatorCrystal.class, "~", location(0, 2));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 1);
                warnings = errorReporter.getCompilationWarnings();
                testWarning(warnings.get(0), location(0, 4), sourceString, ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines before.
            sourceString = "~\n,~\n,~\n,";

            errorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, errorReporter, 0);

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(1), location(1, 1), ",~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(2), location(2, 1), ",~", ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas with newlines after case is invalid.
            sourceString = "~,\n~,\n~,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), DeleteOperatorCrystal.class, "~", location(0, 0));
            testCrystal(statements.get(1).get(0), DeleteOperatorCrystal.class, "~", location(1, 0));
            testCrystal(statements.get(2).get(0), DeleteOperatorCrystal.class, "~", location(2, 0));

            assertNoWarnings(errorReporter);

            // Terminated by commas with newlines before and after.
            sourceString = "~\n,\n~\n,\n~\n,\n";

            errorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, errorReporter, 0);

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(1), location(2, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(2), location(4, 0), "~", ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_Warnings_LineContinuation_ThreeStatements_OnlyMultipleLineContinuations() {
        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;
            LexerTestUtils.setEnableWarnings(warningsEnabled);

            // Terminated by newlines.
            String sourceString = "~\n~\n~\n\n" +
                    "~\n~\n~\n\n" +
                    "~\n~\n~\n\n";

            SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, errorReporter, 0);

            List<VikariError> warnings;

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(1), location(4, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(2), location(8, 0), "~", ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas case is invalid.
            sourceString = "~\n~\n~," +
                    "~\n~\n~," +
                    "~\n~\n~,";

            errorReporter = new SyntaxErrorReporter();
            List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 0);

            testCrystal(statements.get(0).get(0), DeleteOperatorCrystal.class, "~", location(2, 0));
            testCrystal(statements.get(1).get(0), DeleteOperatorCrystal.class, "~", location(4, 0));
            testCrystal(statements.get(2).get(0), DeleteOperatorCrystal.class, "~", location(6, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(2, 2), "~,~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(4, 2), "~,~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines before.
            sourceString = "~\n~\n~\n," +
                    "~\n~\n~\n," +
                    "~\n~\n~\n,";

            errorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, errorReporter, 0);

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(1), location(3, 1), ",~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(2), location(6, 1), ",~", ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }

            // NOTE: The terminated by commas with newlines after case is invalid.
            sourceString = "~\n~\n~,\n" +
                    "~\n~\n~,\n" +
                    "~\n~\n~,\n";

            errorReporter = new SyntaxErrorReporter();
            statements = lex(sourceString, 3, errorReporter, 0, crystalCounts(1, 1, 1));

            testCrystal(statements.get(0).get(0), DeleteOperatorCrystal.class, "~", location(2, 0));
            testCrystal(statements.get(1).get(0), DeleteOperatorCrystal.class, "~", location(5, 0));
            testCrystal(statements.get(2).get(0), DeleteOperatorCrystal.class, "~", location(8, 0));

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(1), location(3, 0), "~", START_OF_STATEMENT);
                testWarning(warnings.get(2), location(6, 0), "~", START_OF_STATEMENT);
            } else {
                assertNoWarnings(errorReporter);
            }

            // Terminated by commas with newlines before and after.
            sourceString = "~\n~\n~\n,\n" +
                    "~\n~\n~\n,\n" +
                    "~\n~\n~\n,\n";

            errorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, errorReporter, 0);

            if (warningsEnabled) {
                assertWarnings(errorReporter, 3);
                warnings = errorReporter.getCompilationWarnings();

                testWarning(warnings.get(0), location(0, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(1), location(4, 0), "~", ONLY_LINE_CONTINUATIONS);
                testWarning(warnings.get(2), location(8, 0), "~", ONLY_LINE_CONTINUATIONS);
            } else {
                assertNoWarnings(errorReporter);
            }
        }
    }
}
