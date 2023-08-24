package com.atonementcrystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_PrintStatements extends TreeWalkInterpreterPrintTest_Base {

    /**
     * Helper method to efficiently test Vikari print statements.
     * @param sourceString The Vikari source code to execute.
     * @param expectedOutput The expected output for the print statements.
     */
    public void testPrintStatement(String sourceString, String expectedOutput) {
        lexParseAndInterpret(sourceString);
        testOutput(expectedOutput);
    }

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_PrintExpression_Empty() {
        testPrintStatement(":", "\n");
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_PrintExpression_Simple() {
        testPrintStatement(":5", "5");
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_PrintExpression_Chained() {
        testPrintStatement(":5:3:7", "537");
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_PrintlnExpression_Simple() {
        testPrintStatement(":5:", "5\n");
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_PrintlnExpression_Chained() {
        testPrintStatement(":5:3:7:", "537\n");
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_PrintExpression_BinaryExpression() {
        testPrintStatement(":5 + 3", "8");
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_PrintlnExpression_BinaryExpression() {
        testPrintStatement(":5 + 3:", "8\n");
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_PrintlnExpression_BinaryExpression_Chained() {
        testPrintStatement(":5 + 3:7 - 2:", "85\n");
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_PrintExpression_MultiLine() {
        testPrintStatement(":5 + 3\n:7 - 2", "85");
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_PrintlnExpression_MultiLine() {
        testPrintStatement(":5 + 3:\n:7 - 2:", "8\n5\n");
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_PrintExpression_Chained_MultiLine() {
        testPrintStatement(":5 + 3\n:7 - 2\n:\n:22:7", "85\n227");
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_PrintlnExpression_Chained_MultiLine() {
        testPrintStatement(":5 + 3:\n:7 - 2:\n:\n:22:7:", "8\n5\n\n227\n");
    }

    @Test
    @Order(13)
    public void testTreeWalkInterpreter_PrintExpression_BigDecimal_StringRepresentation() {
        testPrintStatement(":10.0B", "10.0");
        testPrintStatement(":10.000B", "10.0");
        testPrintStatement(":10.1B", "10.1");
        testPrintStatement(":10.10B", "10.1");
        testPrintStatement(":10.001B", "10.001");
        testPrintStatement(":10.00100B", "10.001");
        testPrintStatement(":10.111B", "10.111");
        testPrintStatement(":10.111000B", "10.111");
    }
}
