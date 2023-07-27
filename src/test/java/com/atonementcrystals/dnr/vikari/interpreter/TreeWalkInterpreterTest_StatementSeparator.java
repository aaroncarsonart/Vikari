package com.atonementcrystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_StatementSeparator extends TreeWalkInterpreterPrintTest_Base {

    /**
     * Helper method to efficiently test Vikari print statements.
     * @param sourceString The Vikari source code to execute.
     * @param expectedOutput The expected output for the print statements.
     */
    public void testPrintStatement(String sourceString, String expectedOutput) {
        lexParseAndInterpret(sourceString);
        testOutput(expectedOutput);
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

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_StatementSeparator_PrintlnExpression_MultipleStatementSeparators() {
        String expectedOutput = "8\n5\n27\n";

        // 1. Single statement separator.
        testPrintStatement(",:5 + 3:,:7 - 2:,:9 * 3:,", expectedOutput);
        testPrintStatement("\n,:5 + 3:\n,:7 - 2:\n,:9 * 3:\n,", expectedOutput);
        testPrintStatement(",\n:5 + 3:,\n:7 - 2:,\n:9 * 3:,\n", expectedOutput);
        testPrintStatement("\n,\n:5 + 3:\n,\n:7 - 2:\n,\n:9 * 3:\n,\n", expectedOutput);

        // 2. Two statement separators.
        testPrintStatement(",,:5 + 3:,,:7 - 2:,,:9 * 3:,,", expectedOutput);
        testPrintStatement("\n,,:5 + 3:\n,,:7 - 2:\n,,:9 * 3:\n,,", expectedOutput);
        testPrintStatement(",,\n:5 + 3:,,\n:7 - 2:,,\n:9 * 3:,,\n", expectedOutput);
        testPrintStatement("\n,,\n:5 + 3:\n,,\n:7 - 2:\n,,\n:9 * 3:\n,,\n", expectedOutput);

        // 3. Four statement separators.
        testPrintStatement(",,,,:5 + 3:,,,,:7 - 2:,,,,:9 * 3:,,,,", expectedOutput);
        testPrintStatement("\n,,,,:5 + 3:\n,,,,:7 - 2:\n,,,,:9 * 3:\n,,,,", expectedOutput);
        testPrintStatement(",,,,\n:5 + 3:,,,,\n:7 - 2:,,,,\n:9 * 3:,,,,\n", expectedOutput);
        testPrintStatement("\n,,,,\n:5 + 3:\n,,,,\n:7 - 2:\n,,,,\n:9 * 3:\n,,,,\n", expectedOutput);
    }
}
