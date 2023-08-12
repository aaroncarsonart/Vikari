package com.atonementcrystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_LogicalOperators extends TreeWalkInterpreterPrintTest_Base {

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_LogicalOperators_TruthTable_And() {
        testBooleanExpression("true ^ true", true);
        testBooleanExpression("false ^ true", false);
        testBooleanExpression("true ^ false", false);
        testBooleanExpression("false ^ false", false);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_LogicalOperators_TruthTable_Or() {
        testBooleanExpression("true \" true", true);
        testBooleanExpression("false \" true", true);
        testBooleanExpression("true \" false", true);
        testBooleanExpression("false \" false", false);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_LogicalOperators_TruthTable_Not() {
        testBooleanExpression("'true", false);
        testBooleanExpression("'false", true);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_LogicalOperators_ComplexExpressions() {
        testBooleanExpression("true ^ false \" true", true);
        testBooleanExpression("false \" true ^ true", true);
        testBooleanExpression("false \" false \" false \" true", true);
        testBooleanExpression("true ^ true ^ true ^ false", false);
    }
}
