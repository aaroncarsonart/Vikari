package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Base;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Grouping extends ParserTest_Base {

    @Test
    @Order(1)
    public void testParser_Expression_Grouping_IntegerLiteral() {
        String sourceString = "[5]";
        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        // statements
        GroupingExpression groupingExpression = assertGroupingExpression(statements.get(0), location(0, 0));
        testLiteralExpression(groupingExpression.getExpression(), IntegerCrystal.class, 5, location(0, 1));
    }

    @Test
    @Order(2)
    public void testParser_Expression_Grouping_BinaryExpression() {
        String sourceString = "[22 / 7]";
        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        // statements
        GroupingExpression groupingExpression = assertGroupingExpression(statements.get(0), location(0, 0));
        testBinaryExpression(groupingExpression.getExpression(), LeftDivideOperatorCrystal.class, 22, 7);
    }

    @Test
    @Order(3)
    public void testParser_Expression_Grouping_MissingOpeningSquareBracket() {
        String sourceString = "22 / 7]";
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 1);
        assertStatementCount(statements, 1);

        // Statements
        ExpressionStatement expressionStatement = assertExpressionStatement(statements.get(0), location(0, 0));
        testBinaryExpression(expressionStatement.getExpression(), LeftDivideOperatorCrystal.class, 22, 7);

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 6), sourceString, "Expected expression.");
    }

    @Test
    @Order(4)
    public void testParser_Expression_Grouping_MissingClosingSquareBracket_Factor() {
        String sourceString = "[22 / 7";
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 1);
        assertStatementCount(statements, 1);

        // Statements
        testSyntaxErrorStatement(statements.get(0), "[22/7", location(0, 0));

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 7), sourceString, "Expected `]` after expression.");
    }

    @Test
    @Order(5)
    public void testParser_Expression_Grouping_MissingClosingSquareBracket_Term() {
        String sourceString = "[3 - 2";
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 1);
        assertStatementCount(statements, 1);

        // Statements
        testSyntaxErrorStatement(statements.get(0), "[3-2", location(0, 0));

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 6), sourceString, "Expected `]` after expression.");
    }

    @Test
    @Order(6)
    public void testParser_Expression_Grouping_EmptyGrouping() {
        String sourceString = "[]";
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 1);
        assertStatementCount(statements, 1);

        // Statements
        testSyntaxErrorStatement(statements.get(0), "[]", location(0, 0));

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Expected expression.");
    }

    @Test
    @Order(7)
    public void testParser_Expression_Grouping_BareSingleBrackets() {
        List<String> sourceStrings = List.of("[", "]");
        for (String sourceString : sourceStrings) {
            List<Statement> statements = lexAndParse_WithErrors(sourceString, 1);
            assertStatementCount(statements, 1);

            // Statements
            testSyntaxErrorStatement(statements.get(0), sourceString, location(0, 0));

            // Syntax Errors
            List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
            testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Expected expression.");
        }
    }

    @Test
    @Order(8)
    public void testParser_Expression_Grouping_MissingOpeningSquareBracket_twoLines() {
        String sourceString = "3 - 2]\n" +
                              "3 - 2]";

        List<Statement> statements = lexAndParse_WithErrors(sourceString, 2);
        assertStatementCount(statements, 2);

        // Statements
        for (int i = 0; i < 2; i++) {
            ExpressionStatement expressionStatement = assertExpressionStatement(statements.get(i), location(i, 0));
            testBinaryExpression(expressionStatement.getExpression(), SubtractOperatorCrystal.class, 3, 2);
        }

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 5), "3 - 2]", "Expected expression.");
        testSyntaxError(syntaxErrors.get(1), location(1, 5), "3 - 2]", "Expected expression.");
    }

    @Test
    @Order(9)
    public void testParser_Expression_Grouping_MissingClosingSquareBracket_TwoLines() {
        String sourceString = "[3 - 2\n" +
                              "[3 - 2";

        List<Statement> statements = lexAndParse_WithErrors(sourceString, 2);
        assertStatementCount(statements, 2);

        // Statements
        testSyntaxErrorStatement(statements.get(0), "[3-2", location(0, 0));
        testSyntaxErrorStatement(statements.get(1), "[3-2", location(1, 0));

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 6), "[3 - 2", "Expected `]` after expression.");
        testSyntaxError(syntaxErrors.get(1), location(1, 6), "[3 - 2", "Expected `]` after expression.");
    }

    @Test
    @Order(10)
    public void testParser_Expression_Grouping_MissingClosingSquareBracket_LongerEndToken() {
        String sourceString = "[100";
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 1);
        assertStatementCount(statements, 1);

        // Statements
        testSyntaxErrorStatement(statements.get(0), sourceString, location(0, 0));

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 4), sourceString, "Expected `]` after expression.");
    }

    @Test
    @Order(11)
    public void testParser_Expression_Grouping_MissingClosingSquareBracket_LongerEndToken_WithArithmeticExpression() {
        String sourceString = "[200 + 300";
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 1);
        assertStatementCount(statements, 1);

        // Statements
        testSyntaxErrorStatement(statements.get(0), "[200+300", location(0, 0));

        // Syntax Errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 10), sourceString, "Expected `]` after expression.");
    }
}
