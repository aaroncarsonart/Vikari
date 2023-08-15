package com.atonementcrystals.dnr.vikari.parser.statement;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testSyntaxError;
import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.lexAndParse;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_StatementSeparator {

    private List<VikariError> lexAndParse_ErrorCase(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        parser.parse(null, lexedStatements);

        return syntaxErrorReporter.getSyntaxErrors();
    }

    // The following 21 tests demonstrate Vikari code statements that would not be
    // normally be written in production code (a series of sequential arithmetic
    // binary expression statements with no side effects.) But at this point in
    // development, testing a sequence of arithmetic expressions is the most easily
    // verifiable method of testing the output of the Parser. So these tests simply
    // demonstrate that the Parser is correctly accepting statement separators
    // between a series of statements in a variety of combinations and scenarios.

    @Test
    @Order(1)
    public void testParser_Statement_SingleTerminatedStatement() {
        String sourceString = "5 + 2,";
        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        Expression innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, AddOperatorCrystal.class, 5, 2);
    }

    @Test
    @Order(2)
    public void testParser_Statement_SingleTerminatedStatement_LeadingSpace() {
        String sourceString = "3 - 9 ,";
        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        Expression innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, SubtractOperatorCrystal.class, 3, 9);
    }

    @Test
    @Order(3)
    public void testParser_Statement_SingleTerminatedStatement_TrailingSpace() {
        String sourceString = "2 * 8, ";
        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        Expression innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, MultiplyOperatorCrystal.class, 2, 8);
    }

    @Test
    @Order(4)
    public void testParser_Statement_SingleTerminatedStatement_LeadingAndTrailingSpace() {
        String sourceString = "22 / 7 , ";
        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        Expression innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, LeftDivideOperatorCrystal.class, 22, 7);
    }

    private void checkTwoArithmeticExpressionStatements(List<Statement> parsedStatements) {
        int expectedSize = 2;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        Expression innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, SubtractOperatorCrystal.class, 3, 9);

        // statement 2
        statement = parsedStatements.get(1);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, LeftDivideOperatorCrystal.class, 22, 7);
    }

    @Test
    @Order(5)
    public void testParser_Statement_TwoStatements_NoSpaces() {
        String sourceString = "3 - 9,22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkTwoArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(6)
    public void testParser_Statement_TwoStatements_LeadingSpace() {
        String sourceString = "3 - 9 ,22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkTwoArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(7)
    public void testParser_Statement_TwoStatements_TrailingSpace() {
        String sourceString = "3 - 9, 22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkTwoArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(8)
    public void testParser_Statement_TwoStatements_LeadingAndTrailingSpaces() {
        String sourceString = "3 - 9 , 22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkTwoArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(9)
    public void testParser_Statement_MultipleLiteralExpressionStatements() {
        String sourceString = "5,22,7";
        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 3;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        Expression innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testIntegerLiteralExpression(innerExpression, 5);

        // statement 2
        statement = parsedStatements.get(1);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testIntegerLiteralExpression(innerExpression, 22);

        // statement 3
        statement = parsedStatements.get(2);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testIntegerLiteralExpression(innerExpression, 7);
    }

    private void checkFourArithmeticExpressionStatements(List<Statement> parsedStatements) {
        int expectedSize = 4;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        Expression innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, AddOperatorCrystal.class, 5, 2);

        // statement 2
        statement = parsedStatements.get(1);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, SubtractOperatorCrystal.class, 3, 9);

        // statement 3
        statement = parsedStatements.get(2);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, MultiplyOperatorCrystal.class, 2, 8);

        // statement 4
        statement = parsedStatements.get(3);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");

        innerExpression = ((ExpressionStatement) statement).getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, LeftDivideOperatorCrystal.class, 22, 7);
    }

    @Test
    @Order(10)
    public void testParser_Statement_MultipleExpressionStatements() {
        String sourceString = "5 + 2,3 - 9,2 * 8,22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(11)
    public void testParser_Statement_MultipleExpressionStatements_SeparateLines() {
        String sourceString = "5 + 2,\n" +
                "3 - 9,\n" +
                "2 * 8,\n" +
                "22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(12)
    public void testParser_Statement_MultipleExpressionStatements_SeparateLines_LeadingSpaces() {
        String sourceString = "5 + 2 ,\n" +
                "3 - 9 ,\n" +
                "2 * 8 ,\n" +
                "22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(13)
    public void testParser_Statement_MultipleExpressionStatements_SeparateLines_TrailingSpaces() {
        String sourceString = "5 + 2, \n" +
                              "3 - 9, \n" +
                              "2 * 8, \n" +
                              "22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(14)
    public void testParser_Statement_MultipleExpressionStatements_SeparateLines_LeadingAndTrailingSpaces() {
        String sourceString = "5 + 2 , \n" +
                              "3 - 9 , \n" +
                              "2 * 8 , \n" +
                              "22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(15)
    public void testParser_Statement_MultipleExpressionStatements_TwoPerSeparateLine_NoSpaces() {
        String sourceString = "5 + 2,3 - 9\n" +
                              "2 * 8,22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(16)
    public void testParser_Statement_MultipleExpressionStatements_TwoPerSeparateLine_LeadingSpaces() {
        String sourceString = "5 + 2 ,3 - 9\n" +
                              "2 * 8 ,22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(17)
    public void testParser_Statement_MultipleExpressionStatements_TwoPerSeparateLine_TrailingSpaces() {
        String sourceString = "5 + 2, 3 - 9\n" +
                              "2 * 8, 22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(18)
    public void testParser_Statement_MultipleExpressionStatements_TwoPerSeparateLine_LeadingAndTrailingSpaces() {
        String sourceString = "5 + 2,3 - 9\n" +
                "2 * 8,22 / 7";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }

    @Test
    @Order(19)
    public void testParser_Statement_syntaxError_SingleStatement() {
        String sourceString = "5 +,";
        List<VikariError> syntaxErrors = lexAndParse_ErrorCase(sourceString);

        int expectedSize = 1;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        VikariError error1 = syntaxErrors.get(0);
        CoordinatePair expectedLocation = location(0, 2);
        String expectedLine = sourceString;
        testSyntaxError(error1, expectedLocation, expectedLine, "Expected expression.");
    }

    @Test
    @Order(20)
    public void testParser_Statement_syntaxError_TwoStatements_SingleLine() {
        String sourceString = "5 +,* 7,";
        List<VikariError> syntaxErrors = lexAndParse_ErrorCase(sourceString);

        int expectedSize = 2;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        VikariError error1 = syntaxErrors.get(0);
        CoordinatePair expectedLocation = location(0, 2);
        String expectedLine = sourceString;
        testSyntaxError(error1, expectedLocation, expectedLine, "Expected expression.");

        VikariError error2 = syntaxErrors.get(1);
        expectedLocation = location(0, 4);
        expectedLine = sourceString;
        testSyntaxError(error2, expectedLocation, expectedLine, "Expected expression.");
    }

    @Test
    @Order(21)
    public void testParser_Statement_syntaxError_FourStatements_MultipleLines() {
        String sourceString = "5 ++, * 7 *,\n" +
                              "22 -, / 3";
        List<VikariError> syntaxErrors = lexAndParse_ErrorCase(sourceString);

        int expectedSize = 4;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        VikariError error1 = syntaxErrors.get(0);
        CoordinatePair expectedLocation = location(0, 2);
        String expectedLine = "5 ++, * 7 *,";
        testSyntaxError(error1, expectedLocation, expectedLine, "Expected expression.");

        VikariError error2 = syntaxErrors.get(1);
        expectedLocation = location(0, 6);
        expectedLine = "5 ++, * 7 *,";
        testSyntaxError(error2, expectedLocation, expectedLine, "Expected expression.");

        VikariError error3 = syntaxErrors.get(2);
        expectedLocation = location(1, 3);
        expectedLine = "22 -, / 3";
        testSyntaxError(error3, expectedLocation, expectedLine, "Expected expression.");

        VikariError error4 = syntaxErrors.get(3);
        expectedLocation = location(1, 6);
        expectedLine = "22 -, / 3";
        testSyntaxError(error4, expectedLocation, expectedLine, "Expected expression.");
    }

    @Test
    @Order(22)
    public void testParser_Statement_MultipleStatementSeparators() {
        // 1. Single statement separator.
        String sourceString = ",5 + 2,3 - 9,2 * 8,22 / 7,";
        List<Statement> parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = "\n,5 + 2\n,3 - 9\n,2 * 8\n,22 / 7\n,";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = ",\n5 + 2,\n3 - 9,\n2 * 8,\n22 / 7,\n";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = "\n,\n5 + 2\n,\n3 - 9\n,\n2 * 8\n,\n22 / 7\n,\n";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        // 2. Two statement separators.
        sourceString = ",,5 + 2,,3 - 9,,2 * 8,,22 / 7,,";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = "\n,,5 + 2\n,,3 - 9\n,,2 * 8\n,,22 / 7\n,,";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = ",,\n5 + 2,,\n3 - 9,,\n2 * 8,,\n22 / 7,,\n";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = "\n,,\n5 + 2\n,,\n3 - 9\n,,\n2 * 8\n,,\n22 / 7\n,,\n";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        // 2. Four statement separators.
        sourceString = ",,,,5 + 2,,,,3 - 9,,,,2 * 8,,,,22 / 7,,,,";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = "\n,,,,5 + 2\n,,,,3 - 9\n,,,,2 * 8\n,,,,22 / 7\n,,,,";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = ",,,,\n5 + 2,,,,\n3 - 9,,,,\n2 * 8,,,,\n22 / 7,,,,\n";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);

        sourceString = "\n,,,,\n5 + 2\n,,,,\n3 - 9\n,,,,\n2 * 8\n,,,,\n22 / 7\n,,,,\n";
        parsedStatements = lexAndParse(sourceString);
        checkFourArithmeticExpressionStatements(parsedStatements);
    }
}
