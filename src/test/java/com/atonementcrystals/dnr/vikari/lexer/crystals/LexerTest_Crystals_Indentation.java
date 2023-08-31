package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.DeleteOperatorCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_Indentation {

    @Test
    @Order(1)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_Spaces() {
        String sourceString = "    foo:Integer << 2";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5, errorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 4));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 7));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, 8));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, 16));
        testCrystal(statement.get(4), IntegerCrystal.class, "2", location(0, 19));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 4), sourceString, "Unexpected indentation level.");
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_Spaces_MultipleLines() {
        String sourceString = """
                  foo << 2
                  :foo:
                  ~foo
                """;

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 3, crystalCounts(3, 3, 2));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 2));
        testCrystal(statements.get(0).get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 6));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(0, 9));

        testCrystal(statements.get(1).get(0), TypeLabelOperatorCrystal.class, ":", location(1, 2));
        testCrystal(statements.get(1).get(1), ReferenceCrystal.class, "foo", location(1, 3));
        testCrystal(statements.get(1).get(2), TypeLabelOperatorCrystal.class, ":", location(1, 6));

        testCrystal(statements.get(2).get(0), DeleteOperatorCrystal.class, "~", location(2, 2));
        testCrystal(statements.get(2).get(1), ReferenceCrystal.class, "foo", location(2, 3));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        String errorMessage = "Unexpected indentation level.";

        testSyntaxError(syntaxErrors.get(0), location(0, 2), "  foo << 2", errorMessage);
        testSyntaxError(syntaxErrors.get(1), location(1, 2), "  :foo:", errorMessage);
        testSyntaxError(syntaxErrors.get(2), location(2, 2), "  ~foo", errorMessage);
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_Spaces_MultipleLines_AtDifferentLevels() {
        String sourceString = """
                0         ~:Indented zero spaces.:~
                 1        ~:Indented one space.:~
                  2       ~:Indented two spaces.:~
                   3      ~:Indented three spaces.:~
                    4     ~:Indented four spaces.:~
                     5    ~:Indented five spaces.:~
                """;

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 6, errorReporter, 5, crystalCounts(1, 1, 1, 1, 1, 1));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "0", location(0, 0));
        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "1", location(1, 1));
        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "2", location(2, 2));
        testCrystal(statements.get(3).get(0), IntegerCrystal.class, "3", location(3, 3));
        testCrystal(statements.get(4).get(0), IntegerCrystal.class, "4", location(4, 4));
        testCrystal(statements.get(5).get(0), IntegerCrystal.class, "5", location(5, 5));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        String errorMessage = "Unexpected indentation level.";
        String[] lines = sourceString.split("\n");

        testSyntaxError(syntaxErrors.get(0), location(1, 1), lines[1], errorMessage);
        testSyntaxError(syntaxErrors.get(1), location(2, 2), lines[2], errorMessage);
        testSyntaxError(syntaxErrors.get(2), location(3, 3), lines[3], errorMessage);
        testSyntaxError(syntaxErrors.get(3), location(4, 4), lines[4], errorMessage);
        testSyntaxError(syntaxErrors.get(4), location(5, 5), lines[5], errorMessage);
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_Tabs() {
        String sourceString = "\tfoo:Integer << 2";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5, errorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 1));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 4));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, 5));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, 13));
        testCrystal(statement.get(4), IntegerCrystal.class, "2", location(0, 16));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Unexpected indentation level.");
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_Tabs_MultipleLines() {
        String sourceString = """
                \t\tfoo << 2
                \t\t:foo:
                \t\t~foo
                """;

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 3, crystalCounts(3, 3, 2));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 2));
        testCrystal(statements.get(0).get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 6));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(0, 9));

        testCrystal(statements.get(1).get(0), TypeLabelOperatorCrystal.class, ":", location(1, 2));
        testCrystal(statements.get(1).get(1), ReferenceCrystal.class, "foo", location(1, 3));
        testCrystal(statements.get(1).get(2), TypeLabelOperatorCrystal.class, ":", location(1, 6));

        testCrystal(statements.get(2).get(0), DeleteOperatorCrystal.class, "~", location(2, 2));
        testCrystal(statements.get(2).get(1), ReferenceCrystal.class, "foo", location(2, 3));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        String errorMessage = "Unexpected indentation level.";

        testSyntaxError(syntaxErrors.get(0), location(0, 2), "\t\tfoo << 2", errorMessage);
        testSyntaxError(syntaxErrors.get(1), location(1, 2), "\t\t:foo:", errorMessage);
        testSyntaxError(syntaxErrors.get(2), location(2, 2), "\t\t~foo", errorMessage);
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_Tabs_MultipleLines_AtDifferentLevels() {
        String sourceString = """
                0              ~:Indented zero tabs.:~
                \t1            ~:Indented one tab.:~
                \t\t2          ~:Indented two tabs.:~
                \t\t\t3        ~:Indented three tabs.:~
                \t\t\t\t4      ~:Indented four tabs.:~
                \t\t\t\t\t5    ~:Indented five tabs.:~
                """;

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 6, errorReporter, 5, crystalCounts(1, 1, 1, 1, 1, 1));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "0", location(0, 0));
        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "1", location(1, 1));
        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "2", location(2, 2));
        testCrystal(statements.get(3).get(0), IntegerCrystal.class, "3", location(3, 3));
        testCrystal(statements.get(4).get(0), IntegerCrystal.class, "4", location(4, 4));
        testCrystal(statements.get(5).get(0), IntegerCrystal.class, "5", location(5, 5));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        String errorMessage = "Unexpected indentation level.";
        String[] lines = sourceString.split("\n");

        testSyntaxError(syntaxErrors.get(0), location(1, 1), lines[1], errorMessage);
        testSyntaxError(syntaxErrors.get(1), location(2, 2), lines[2], errorMessage);
        testSyntaxError(syntaxErrors.get(2), location(3, 3), lines[3], errorMessage);
        testSyntaxError(syntaxErrors.get(3), location(4, 4), lines[4], errorMessage);
        testSyntaxError(syntaxErrors.get(4), location(5, 5), lines[5], errorMessage);
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_TabsAndSpaces() {
        String sourceString = "\t foo:Integer << 2";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5, errorReporter, 1);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 2));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, 5));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, 6));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, 14));
        testCrystal(statement.get(4), IntegerCrystal.class, "2", location(0, 17));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 2), sourceString, "Unexpected indentation level.");
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_TabsAndSpaces_MultipleLines() {
        String sourceString = """
                \t\t  foo << 2
                \t\t  :foo:
                \t\t  ~foo
                """;

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 3, crystalCounts(3, 3, 2));

        testCrystal(statements.get(0).get(0), ReferenceCrystal.class, "foo", location(0, 4));
        testCrystal(statements.get(0).get(1), LeftAssignmentOperatorCrystal.class, "<<", location(0, 8));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(0, 11));

        testCrystal(statements.get(1).get(0), TypeLabelOperatorCrystal.class, ":", location(1, 4));
        testCrystal(statements.get(1).get(1), ReferenceCrystal.class, "foo", location(1, 5));
        testCrystal(statements.get(1).get(2), TypeLabelOperatorCrystal.class, ":", location(1, 8));

        testCrystal(statements.get(2).get(0), DeleteOperatorCrystal.class, "~", location(2, 4));
        testCrystal(statements.get(2).get(1), ReferenceCrystal.class, "foo", location(2, 5));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        String errorMessage = "Unexpected indentation level.";

        testSyntaxError(syntaxErrors.get(0), location(0, 4), "\t\t  foo << 2", errorMessage);
        testSyntaxError(syntaxErrors.get(1), location(1, 4), "\t\t  :foo:", errorMessage);
        testSyntaxError(syntaxErrors.get(2), location(2, 4), "\t\t  ~foo", errorMessage);
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_SyntaxErrors_UnexpectedIndentationLevel_TabsAndSpaces_MultipleLines_AtDifferentLevels() {
        String sourceString = """
                0            ~:Indented by zero spaces and tabs.:~
                \t1          ~:Indented by one tab.:~
                \t 2         ~:Indented by one tab and one space.:~
                 \t\t3       ~:Indented by one space and two tabs.:~
                \t \t 4      ~:Indented by a tab, space, tab, and space.:~
                   \t\t5     ~:Indented by three spaces and two tabs.:~
                """;

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 6, errorReporter, 5, crystalCounts(1, 1, 1, 1, 1, 1));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "0", location(0, 0));
        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "1", location(1, 1));
        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "2", location(2, 2));
        testCrystal(statements.get(3).get(0), IntegerCrystal.class, "3", location(3, 3));
        testCrystal(statements.get(4).get(0), IntegerCrystal.class, "4", location(4, 4));
        testCrystal(statements.get(5).get(0), IntegerCrystal.class, "5", location(5, 5));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        String errorMessage = "Unexpected indentation level.";
        String[] lines = sourceString.split("\n");

        testSyntaxError(syntaxErrors.get(0), location(1, 1), lines[1], errorMessage);
        testSyntaxError(syntaxErrors.get(1), location(2, 2), lines[2], errorMessage);
        testSyntaxError(syntaxErrors.get(2), location(3, 3), lines[3], errorMessage);
        testSyntaxError(syntaxErrors.get(3), location(4, 4), lines[4], errorMessage);
        testSyntaxError(syntaxErrors.get(4), location(5, 5), lines[5], errorMessage);
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_SyntaxErrors_Indentation_AfterLineContinuation() {
        String sourceString = """
                ~:Spaces.:~
                2~
                  + 2

                ~:Tabs.:~
                3~
                \t+ 3

                ~:Spaces and tabs.:~
                4~
                \t\t  + 4
                """;
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(3, 3, 3));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "2", location(1, 0));
        testCrystal(statements.get(0).get(1), AddOperatorCrystal.class, "+", location(2, 2));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(2, 4));

        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "3", location(5, 0));
        testCrystal(statements.get(1).get(1), AddOperatorCrystal.class, "+", location(6, 1));
        testCrystal(statements.get(1).get(2), IntegerCrystal.class, "3", location(6, 3));

        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "4", location(9, 0));
        testCrystal(statements.get(2).get(1), AddOperatorCrystal.class, "+", location(10, 4));
        testCrystal(statements.get(2).get(2), IntegerCrystal.class, "4", location(10, 6));
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_SyntaxErrors_Indentation_AfterEmptyLineContinuation() {
        String sourceString = """
                ~:Spaces.:~
                ~
                  2 + 2

                ~:Tabs.:~
                ~
                \t3 + 3

                ~:Spaces and tabs.:~
                ~
                \t\t  4 + 4
                """;
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(3, 3, 3));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "2", location(2, 2));
        testCrystal(statements.get(0).get(1), AddOperatorCrystal.class, "+", location(2, 4));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(2, 6));

        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "3", location(6, 1));
        testCrystal(statements.get(1).get(1), AddOperatorCrystal.class, "+", location(6, 3));
        testCrystal(statements.get(1).get(2), IntegerCrystal.class, "3", location(6, 5));

        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "4", location(10, 4));
        testCrystal(statements.get(2).get(1), AddOperatorCrystal.class, "+", location(10, 6));
        testCrystal(statements.get(2).get(2), IntegerCrystal.class, "4", location(10, 8));
    }

    // TODO: Finish this test. (And other necessary test case variants.)

    @Test
    @Order(12)
    public void testLexer_Crystals_SyntaxErrors_Indentation_AfterMultipleContinuations_AtSameLevel() {
        String sourceString = """
                ~:Spaces.:~
                1~
                  +~
                  2~
                  -~
                  3~

                ~:Tabs.:~
                1~
                \t+~
                \t2~
                \t-~
                \t3

                ~:Spaces and tabs.:~
                1~
                \t\t  +~
                \t\t  2~
                \t\t  -~
                \t\t  3
                """;
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(5, 5, 5));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "1", location(1, 0));
        testCrystal(statements.get(0).get(1), AddOperatorCrystal.class, "+", location(2, 2));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(3, 2));
        testCrystal(statements.get(0).get(3), SubtractOperatorCrystal.class, "-", location(4, 2));
        testCrystal(statements.get(0).get(4), IntegerCrystal.class, "3", location(5, 2));

        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "1", location(8, 0));
        testCrystal(statements.get(1).get(1), AddOperatorCrystal.class, "+", location(9, 1));
        testCrystal(statements.get(1).get(2), IntegerCrystal.class, "2", location(10, 1));
        testCrystal(statements.get(1).get(3), SubtractOperatorCrystal.class, "-", location(11, 1));
        testCrystal(statements.get(1).get(4), IntegerCrystal.class, "3", location(12, 1));

        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "1", location(15, 0));
        testCrystal(statements.get(2).get(1), AddOperatorCrystal.class, "+", location(16, 4));
        testCrystal(statements.get(2).get(2), IntegerCrystal.class, "2", location(17, 4));
        testCrystal(statements.get(2).get(3), SubtractOperatorCrystal.class, "-", location(18, 4));
        testCrystal(statements.get(2).get(4), IntegerCrystal.class, "3", location(19, 4));
    }

    @Test
    @Order(13)
    public void testLexer_Crystals_SyntaxErrors_Indentation_AfterMultipleContinuations_AtDifferentLevel() {
        String sourceString = """
                ~:Spaces.:~
                1~
                 +~
                  2~
                   -~
                    3~

                ~:Tabs.:~
                1~
                \t+~
                \t\t2~
                \t\t\t-~
                \t\t\t\t3

                ~:Spaces and tabs.:~
                1~
                \t\t  +~
                \t\t  \t\t  2~
                \t\t  \t\t  \t\t  -~
                \t\t  \t\t  \t\t  \t\t  3
                """;
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, crystalCounts(5, 5, 5));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "1", location(1, 0));
        testCrystal(statements.get(0).get(1), AddOperatorCrystal.class, "+", location(2, 1));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(3, 2));
        testCrystal(statements.get(0).get(3), SubtractOperatorCrystal.class, "-", location(4, 3));
        testCrystal(statements.get(0).get(4), IntegerCrystal.class, "3", location(5, 4));

        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "1", location(8, 0));
        testCrystal(statements.get(1).get(1), AddOperatorCrystal.class, "+", location(9, 1));
        testCrystal(statements.get(1).get(2), IntegerCrystal.class, "2", location(10, 2));
        testCrystal(statements.get(1).get(3), SubtractOperatorCrystal.class, "-", location(11, 3));
        testCrystal(statements.get(1).get(4), IntegerCrystal.class, "3", location(12, 4));

        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "1", location(15, 0));
        testCrystal(statements.get(2).get(1), AddOperatorCrystal.class, "+", location(16, 4));
        testCrystal(statements.get(2).get(2), IntegerCrystal.class, "2", location(17, 8));
        testCrystal(statements.get(2).get(3), SubtractOperatorCrystal.class, "-", location(18, 12));
        testCrystal(statements.get(2).get(4), IntegerCrystal.class, "3", location(19, 16));
    }

    @Test
    @Order(14)
    public void testLexer_Crystals_SyntaxErrors_Indentation_OnLineWithLineContinuation() {
        String sourceString = """
                ~:Spaces.:~
                  2 ~
                  + 2

                ~:Tabs.:~
                \t3 ~
                \t+ 3

                ~:Spaces and tabs.:~
                \t\t  4 ~
                \t\t  + 4
                """;

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<List<AtonementCrystal>> statements = lex(sourceString, 3, errorReporter, 3, crystalCounts(3, 3, 3));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "2", location(1, 2));
        testCrystal(statements.get(0).get(1), AddOperatorCrystal.class, "+", location(2, 2));
        testCrystal(statements.get(0).get(2), IntegerCrystal.class, "2", location(2, 4));

        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "3", location(5, 1));
        testCrystal(statements.get(1).get(1), AddOperatorCrystal.class, "+", location(6, 1));
        testCrystal(statements.get(1).get(2), IntegerCrystal.class, "3", location(6, 3));

        testCrystal(statements.get(2).get(0), IntegerCrystal.class, "4", location(9, 4));
        testCrystal(statements.get(2).get(1), AddOperatorCrystal.class, "+", location(10, 4));
        testCrystal(statements.get(2).get(2), IntegerCrystal.class, "4", location(10, 6));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        String errorMessage = "Unexpected indentation level.";

        testSyntaxError(syntaxErrors.get(0), location(1, 2), "  2 ~", errorMessage);
        testSyntaxError(syntaxErrors.get(1), location(5, 1), "\t3 ~", errorMessage);
        testSyntaxError(syntaxErrors.get(2), location(9, 4), "\t\t  4 ~", errorMessage);
    }
}
