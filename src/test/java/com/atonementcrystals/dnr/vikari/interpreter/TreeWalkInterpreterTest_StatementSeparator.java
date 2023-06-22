package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_StatementSeparator {

    private final PrintStream originalOut = System.out;
    private final ByteArrayOutputStream testOut = new ByteArrayOutputStream();

    @BeforeEach
    public void setupPrintStream() {
        System.setOut(new PrintStream(testOut));
    }

    @AfterEach
    public void restorePrintStream() {
        System.setOut(originalOut);
    }

    /**
     * Helper method to efficiently test Vikari print statements.
     * @param sourceString The Vikari source code to execute.
     * @param expectedOutput The expected output for the print statements.
     */
    public void testPrintStatement(String sourceString, String expectedOutput) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLine);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        for (Statement statement : parsedStatements) {
            interpreter.execute(statement);
        }
        String actualOutput = testOut.toString();
        assertEquals(expectedOutput, actualOutput, "Unexpected output of print statement.");
    }

    // The following 12 tests are all identical to TreeWalkInterpreterTest_PrintStatements,
    // except that the statement separator , has been interpersed between their calls.
    // Resulting behavior should be exactly equivalent in both cases.

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_StatementSeparator_PrintExpression_Empty() {
        testPrintStatement(":,", "\n");
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_StatementSeparator_PrintExpression_Simple() {
        testPrintStatement(":5,", "5");
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_StatementSeparator_PrintExpression_Chained() {
        testPrintStatement(":5,:3,:7", "537");
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_StatementSeparator_PrintlnExpression_Simple() {
        testPrintStatement(":5,:", "5\n");
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_StatementSeparator_PrintlnExpression_Chained() {
        testPrintStatement(":5,:3,:7,:", "537\n");
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_StatementSeparator_PrintExpression_BinaryExpression() {
        testPrintStatement(":5 + 3,", "8");
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_StatementSeparator_PrintlnExpression_BinaryExpression() {
        testPrintStatement(":5 + 3,:,", "8\n");
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_StatementSeparator_PrintlnExpression_BinaryExpression_Chained() {
        testPrintStatement(":5 + 3:7 - 2:,", "85\n");
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_StatementSeparator_PrintExpression_MultiLine() {
        testPrintStatement(":5 + 3,\n:7 - 2,", "85");
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_StatementSeparator_PrintlnExpression_MultiLine() {
        testPrintStatement(":5 + 3,:\n:7 - 2,:,", "8\n5\n");
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_StatementSeparator_PrintExpression_Chained_MultiLine() {
        testPrintStatement(":5 + 3,\n:7 - 2,\n:\n:22,:7,", "85\n227");
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_StatementSeparator_PrintlnExpression_Chained_MultiLine() {
        testPrintStatement(":5 + 3,:,\n:7 - 2,:,\n:,\n:22,:7,:,", "8\n5\n\n227\n");
    }
}
