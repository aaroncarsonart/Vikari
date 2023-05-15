package com.atonementcrystals.dnr.vikari.parser.statement;

import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.PrintExpression;
import com.atonementcrystals.dnr.vikari.core.statement.PrintStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.*;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_PrintStatement {
    private static final CoordinatePair COORDINATE_PAIR_ZERO_ZERO = new CoordinatePair(0, 0);

    @Test
    @Order(1)
    public void testEmptyPrintStatement() {
        String sourceString = ":";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        PrintStatement printStatement = (PrintStatement) statement;

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        List<PrintExpression> printExpressions = printStatement.getPrintExpressions();
        expectedSize = 1;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // print expression 1
        PrintExpression printExpression = printExpressions.get(0);
        assertNull(printExpression.getExpression(), "Expected an empty print statement's inner expression to be null.");
    }

    @Test
    @Order(2)
    public void testSimplePrintStatement() {
        String sourceString = ":2";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        PrintStatement printStatement = (PrintStatement) statement;

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        List<PrintExpression> printExpressions = printStatement.getPrintExpressions();
        expectedSize = 1;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // print expression 1
        PrintExpression printExpression = printExpressions.get(0);
        Expression innerExpression = printExpression.getExpression();
        assertEquals(LiteralExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        testIntegerLiteralExpression(innerExpression, 2);
    }

    @Test
    @Order(3)
    public void testPrintStatement_WithNewline() {
        String sourceString = ":12:";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        PrintStatement printStatement = (PrintStatement) statement;

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        List<PrintExpression> printExpressions = printStatement.getPrintExpressions();
        expectedSize = 2;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // print expression 1
        PrintExpression printExpression = printExpressions.get(0);
        Expression innerExpression = printExpression.getExpression();
        assertEquals(LiteralExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        testIntegerLiteralExpression(innerExpression, 12);

        // print expression 2
        printExpression = printExpressions.get(1);
        innerExpression = printExpression.getExpression();
        assertNull(innerExpression, "Unexpected trailing print operator : to have a null inner expression.");
    }

    @Test
    @Order(4)
    public void testPrintStatement_TermExpression() {
        String sourceString = ":12 + 24:";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        PrintStatement printStatement = (PrintStatement) statement;

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        List<PrintExpression> printExpressions = printStatement.getPrintExpressions();
        expectedSize = 2;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // print expression 1
        PrintExpression printExpression = printExpressions.get(0);
        Expression innerExpression = printExpression.getExpression();
        testBinaryExpression(innerExpression, AddOperatorCrystal.class, 12, 24);

        // print expression 2
        printExpression = printExpressions.get(1);
        innerExpression = printExpression.getExpression();
        assertNull(innerExpression, "Unexpected trailing print operator : to have a null inner expression.");
    }

    @Test
    @Order(5)
    public void testPrintStatement_ThreePrintExpressions() {
        String sourceString = ":1:2:3";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        PrintStatement printStatement = (PrintStatement) statement;

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        List<PrintExpression> printExpressions = printStatement.getPrintExpressions();
        expectedSize = 3;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        List<Integer> expectedValues = List.of(1, 2, 3);

        // test expressions
        for (int i = 0; i < 3; i++) {
            PrintExpression printExpression = printExpressions.get(i);
            Expression innerExpression = printExpression.getExpression();
            Integer expectedValue = expectedValues.get(i);
            testIntegerLiteralExpression(innerExpression, expectedValue);
        }
    }

    @Test
    @Order(6)
    public void testPrintStatement_Grouping() {
        String sourceString = ":[3 * 2]";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        PrintStatement printStatement = (PrintStatement) statement;

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        List<PrintExpression> printExpressions = printStatement.getPrintExpressions();
        expectedSize = 1;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // expression 1 (grouping: [3 * 2])
        PrintExpression printExpression = printExpressions.get(0);
        Expression innerExpression = printExpression.getExpression();
        assertEquals(GroupingExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        GroupingExpression groupingExpression = (GroupingExpression) innerExpression;
        innerExpression = groupingExpression.getExpression();
        testBinaryExpression(innerExpression, MultiplyOperatorCrystal.class, 3, 2);
    }

    @Test
    @Order(7)
    public void testPrintStatement_MultipleLines() {
        String sourceString = ":1:2 - 3:\n" +
                              ":[12 / 7]:\n" +
                              ":4:5:6\n" +
                              ":";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 4;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // -----------
        // statement 1
        // -----------
        Statement statement = parsedStatements.get(0);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        PrintStatement printStatement = (PrintStatement) statement;

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        List<PrintExpression> printExpressions = printStatement.getPrintExpressions();
        expectedSize = 3;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // expression 1 (literal: 1)
        PrintExpression printExpression = printExpressions.get(0);
        Expression innerExpression = printExpression.getExpression();
        testIntegerLiteralExpression(innerExpression, 1);

        // expression 2 (binary: 2 - 3)
        printExpression = printExpressions.get(1);
        innerExpression = printExpression.getExpression();
        testBinaryExpression(innerExpression, SubtractOperatorCrystal.class, 2, 3);

        // expression 3 (empty print expression)
        printExpression = printExpressions.get(2);
        innerExpression = printExpression.getExpression();
        assertNull(innerExpression, "Expected an empty print statement's inner expression to be null.");

        // -----------
        // statement 2
        // -----------
        statement = parsedStatements.get(1);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        printStatement = (PrintStatement) statement;

        expectedLocation = new CoordinatePair(1, 0);
        actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        printExpressions = printStatement.getPrintExpressions();
        expectedSize = 2;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // expression 1 (grouping: [12 / 7])
        printExpression = printExpressions.get(0);
        innerExpression = printExpression.getExpression();
        assertEquals(GroupingExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        GroupingExpression groupingExpression = (GroupingExpression) innerExpression;
        innerExpression = groupingExpression.getExpression();
        testBinaryExpression(innerExpression, LeftDivideOperatorCrystal.class, 12, 7);

        // expression 2 (null)
        printExpression = printExpressions.get(1);
        assertNull(printExpression.getExpression(), "Expected an trailing print operator's inner expression to be " +
                "null.");

        // -----------
        // statement 3
        // -----------
        statement = parsedStatements.get(2);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        printStatement = (PrintStatement) statement;

        expectedLocation = new CoordinatePair(2, 0);
        actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        printExpressions = printStatement.getPrintExpressions();
        expectedSize = 3;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // expression 1 (literal: 4)
        printExpression = printExpressions.get(0);
        innerExpression = printExpression.getExpression();
        testIntegerLiteralExpression(innerExpression, 4);

        // expression 1 (literal: 5)
        printExpression = printExpressions.get(1);
        innerExpression = printExpression.getExpression();
        testIntegerLiteralExpression(innerExpression, 5);

        // expression 1 (literal: 6)
        printExpression = printExpressions.get(2);
        innerExpression = printExpression.getExpression();
        testIntegerLiteralExpression(innerExpression, 6);

        // -----------
        // statement 4
        // -----------
        statement = parsedStatements.get(3);
        assertEquals(PrintStatement.class, statement.getClass(), "Unexpected statement type.");
        printStatement = (PrintStatement) statement;

        expectedLocation = new CoordinatePair(3, 0);
        actualLocation = statement.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected coordinates for statement.");

        printExpressions = printStatement.getPrintExpressions();
        expectedSize = 1;
        actualSize = printExpressions.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of print statements.");

        // expression 1 (null)
        printExpression = printExpressions.get(0);
        assertNull(printExpression.getExpression(), "Expected an empty print statement's inner expression to be null.");
    }
}
