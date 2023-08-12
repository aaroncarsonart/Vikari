package com.atonementcrystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_EqualityOperators extends TreeWalkInterpreterPrintTest_Base {

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_EqualityOperators_Numbers_SimpleIntegerOperands() {
        testBooleanExpression("1 = 1", true);
        testBooleanExpression("1 = 2", false);
        testBooleanExpression("1 '= 1", false);
        testBooleanExpression("1 '= 2", true);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_EqualityOperators_Numbers_SimpleLongOperands() {
        testBooleanExpression("1L = 1L", true);
        testBooleanExpression("1L = 2L", false);
        testBooleanExpression("1L '= 1L", false);
        testBooleanExpression("1L '= 2L", true);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_EqualityOperators_Numbers_SimpleBigIntegerOperands() {
        testBooleanExpression("1B = 1B", true);
        testBooleanExpression("1B = 2B", false);
        testBooleanExpression("1B '= 1B", false);
        testBooleanExpression("1B '= 2B", true);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_EqualityOperators_Numbers_SimpleFloatOperands() {
        testBooleanExpression("1.0F = 1.0F", true);
        testBooleanExpression("1.0F = 2.0F", false);
        testBooleanExpression("1.0F '= 1.0F", false);
        testBooleanExpression("1.0F '= 2.0F", true);
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_EqualityOperators_Numbers_SimpleDoubleOperands() {
        testBooleanExpression("1.0D = 1.0D", true);
        testBooleanExpression("1.0D = 2.0D", false);
        testBooleanExpression("1.0D '= 1.0D", false);
        testBooleanExpression("1.0D '= 2.0D", true);
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_EqualityOperators_Numbers_SimpleBigDecimalOperands() {
        testBooleanExpression("1.0B = 1.0B", true);
        testBooleanExpression("1.0B = 2.0B", false);
        testBooleanExpression("1.0B '= 1.0B", false);
        testBooleanExpression("1.0B '= 2.0B", true);
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_EqualityOperators_Numbers_SimpleIntegerOperands_WithGrouping() {
        testBooleanExpression("[1] = [1]", true);
        testBooleanExpression("[1] = [2]", false);
        testBooleanExpression("[1] '= [1]", false);
        testBooleanExpression("[1] '= [2]", true);
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_EqualityOperators_ArithmeticExpressions() {
        testBooleanExpression("1 + 1 = 4 / 2", true);
        testBooleanExpression("2 * 7 = 28 - 14", true);
        testBooleanExpression("1 + 2 = 12 / 3", false);
        testBooleanExpression("3 * 4 = 15 - 7", false);
        testBooleanExpression("19 - 1 '= 20 + 1", true);
        testBooleanExpression("10 / 2 '= 5 * 3", true);
        testBooleanExpression("315 - 5 '= 155 * 2", false);
        testBooleanExpression("128 / 2 '= 16 * 4", false);
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_EqualityOperators_ArithmeticExpressions_WithGrouping() {
        testBooleanExpression("[1 + 1] = [4 / 2]", true);
        testBooleanExpression("[2 * 7] = [28 - 14]", true);
        testBooleanExpression("[1 + 2] = [12 / 3]", false);
        testBooleanExpression("[3 * 4] = [15 - 7]", false);
        testBooleanExpression("[19 - 1] '= [20 + 1]", true);
        testBooleanExpression("10 / 2 '= 5 * 3", true);
        testBooleanExpression("315 - 5 '= 155 * 2", false);
        testBooleanExpression("128 / 2 '= 16 * 4", false);
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_EqualityOperators_IntegerComparing_NumberOfSameValue_ButDifferentType() {
        //-------
        // Equals
        //-------
        // left operand
        testBooleanExpression("2 = 2L", true);
        testBooleanExpression("2 = 2B", true);
        testBooleanExpression("2 = 2.0F", true);
        testBooleanExpression("2 = 2.0D", true);
        testBooleanExpression("2 = 2.0B", true);

        // right operand
        testBooleanExpression("-5L = -5", true);
        testBooleanExpression("-5B = -5", true);
        testBooleanExpression("-5.0F = -5", true);
        testBooleanExpression("-5.0D = -5", true);
        testBooleanExpression("-5.0B = -5", true);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("2 '= 2L", false);
        testBooleanExpression("2 '= 2B", false);
        testBooleanExpression("2 '= 2.0F", false);
        testBooleanExpression("2 '= 2.0D", false);
        testBooleanExpression("2 '= 2.0B", false);

        // right operand
        testBooleanExpression("-5L '= -5", false);
        testBooleanExpression("-5B '= -5", false);
        testBooleanExpression("-5.0F '= -5", false);
        testBooleanExpression("-5.0D '= -5", false);
        testBooleanExpression("-5.0B '= -5", false);
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_EqualityOperators_IntegerComparing_NumberOfDifferentValue_ButDifferentType() {
        // ------
        // Equals
        // ------
        // left operand
        testBooleanExpression("1 = 2L", false);
        testBooleanExpression("1 = 2B", false);
        testBooleanExpression("1 = 2.0F", false);
        testBooleanExpression("1 = 2.0D", false);
        testBooleanExpression("1 = 2.0B", false);

        // right operand
        testBooleanExpression("-5L = 1", false);
        testBooleanExpression("-5B = 1", false);
        testBooleanExpression("-5.0F = 1", false);
        testBooleanExpression("-5.0D = 1", false);
        testBooleanExpression("-5.0B = 1", false);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("1 '= 2L", true);
        testBooleanExpression("1 '= 2B", true);
        testBooleanExpression("1 '= 2.0F", true);
        testBooleanExpression("1 '= 2.0D", true);
        testBooleanExpression("1 '= 2.0B", true);

        // right operand
        testBooleanExpression("-5L '= 1", true);
        testBooleanExpression("-5B '= 1", true);
        testBooleanExpression("-5.0F '= 1", true);
        testBooleanExpression("-5.0D '= 1", true);
        testBooleanExpression("-5.0B '= 1", true);
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_EqualityOperators_LongComparing_NumberOfSameValue_ButDifferentType() {
        //-------
        // Equals
        //-------
        // left operand
        testBooleanExpression("2L = 2B", true);
        testBooleanExpression("2L = 2.0F", true);
        testBooleanExpression("2L = 2.0D", true);
        testBooleanExpression("2L = 2.0B", true);

        // right operand
        testBooleanExpression("-5B =-5L", true);
        testBooleanExpression("-5.0F =-5L", true);
        testBooleanExpression("-5.0D =-5L", true);
        testBooleanExpression("-5.0B =-5L", true);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("2L '= 2B", false);
        testBooleanExpression("2L '= 2.0F", false);
        testBooleanExpression("2L '= 2.0D", false);
        testBooleanExpression("2L '= 2.0B", false);

        // right operand
        testBooleanExpression("-5B '=-5L", false);
        testBooleanExpression("-5.0F '=-5L", false);
        testBooleanExpression("-5.0D '=-5L", false);
        testBooleanExpression("-5.0B '=-5L", false);
    }

    @Test
    @Order(13)
    public void testTreeWalkInterpreter_EqualityOperators_LongComparing_NumberOfDifferentValue_ButDifferentType() {
        // ------
        // Equals
        // ------
        // left operand
        testBooleanExpression("1L = 2B", false);
        testBooleanExpression("1L = 2.0F", false);
        testBooleanExpression("1L = 2.0D", false);
        testBooleanExpression("1L = 2.0B", false);

        // right operand
        testBooleanExpression("-5B = 1L", false);
        testBooleanExpression("-5.0F = 1L", false);
        testBooleanExpression("-5.0D = 1L", false);
        testBooleanExpression("-5.0B = 1L", false);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("1L '= 2B", true);
        testBooleanExpression("1L '= 2.0F", true);
        testBooleanExpression("1L '= 2.0D", true);
        testBooleanExpression("1L '= 2.0B", true);

        // right operand
        testBooleanExpression("-5B '= 1L", true);
        testBooleanExpression("-5.0F '= 1L", true);
        testBooleanExpression("-5.0D '= 1L", true);
        testBooleanExpression("-5.0B '= 1L", true);
    }

    @Test
    @Order(14)
    public void testTreeWalkInterpreter_EqualityOperators_BigIntegerComparing_NumberOfSameValue_ButDifferentType() {
        //-------
        // Equals
        //-------
        // left operand
        testBooleanExpression("2B = 2.0F", true);
        testBooleanExpression("2B = 2.0D", true);
        testBooleanExpression("2B = 2.0B", true);

        // right operand
        testBooleanExpression("-5.0F = -5B", true);
        testBooleanExpression("-5.0D = -5B", true);
        testBooleanExpression("-5.0B = -5B", true);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("2B '= 2.0F", false);
        testBooleanExpression("2B '= 2.0D", false);
        testBooleanExpression("2B '= 2.0B", false);

        // right operand
        testBooleanExpression("-5.0F '= -5B", false);
        testBooleanExpression("-5.0D '= -5B", false);
        testBooleanExpression("-5.0B '= -5B", false);
    }

    @Test
    @Order(15)
    public void testTreeWalkInterpreter_EqualityOperators_BigIntegerComparing_NumberOfDifferentValue_ButDifferentType() {
        // ------
        // Equals
        // ------
        // left operand
        testBooleanExpression("1B = 2.0F", false);
        testBooleanExpression("1B = 2.0D", false);
        testBooleanExpression("1B = 2.0B", false);

        // right operand
        testBooleanExpression("-5.0F = 1B", false);
        testBooleanExpression("-5.0D = 1B", false);
        testBooleanExpression("-5.0B = 1B", false);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("1B '= 2.0F", true);
        testBooleanExpression("1B '= 2.0D", true);
        testBooleanExpression("1B '= 2.0B", true);

        // right operand
        testBooleanExpression("-5.0F '= 1B", true);
        testBooleanExpression("-5.0D '= 1B", true);
        testBooleanExpression("-5.0B '= 1B", true);
    }

    @Test
    @Order(16)
    public void testTreeWalkInterpreter_EqualityOperators_FloatComparing_NumberOfSameValue_ButDifferentType() {
        //-------
        // Equals
        //-------
        // left operand
        testBooleanExpression("2.0F = 2.0D", true);
        testBooleanExpression("2.0F = 2.0B", true);

        // right operand
        testBooleanExpression("-5.0D = -5.0F", true);
        testBooleanExpression("-5.0B = -5.0F", true);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("2.0F '= 2.0D", false);
        testBooleanExpression("2.0F '= 2.0B", false);

        // right operand
        testBooleanExpression("-5.0D '= -5.0F", false);
        testBooleanExpression("-5.0B '= -5.0F", false);
    }

    @Test
    @Order(17)
    public void testTreeWalkInterpreter_EqualityOperators_FloatComparing_NumberOfDifferentValue_ButDifferentType() {
        // ------
        // Equals
        // ------
        // left operand
        testBooleanExpression("1.0F = 2.0D", false);
        testBooleanExpression("1.0F = 2.0B", false);

        // right operand
        testBooleanExpression("-5.0D = 1.0F", false);
        testBooleanExpression("-5.0B = 1.0F", false);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("1.0F '= 2.0D", true);
        testBooleanExpression("1.0F '= 2.0B", true);

        // right operand
        testBooleanExpression("-5.0D '= 1.0F", true);
        testBooleanExpression("-5.0B '= 1.0F", true);
    }

    @Test
    @Order(18)
    public void testTreeWalkInterpreter_EqualityOperators_DoubleComparing_NumberOfSameValue_ButDifferentType() {
        //-------
        // Equals
        //-------
        // left operand
        testBooleanExpression("2.0D = 2.0B", true);

        // right operand
        testBooleanExpression("-5.0B = -5.0D", true);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("2.0D '= 2.0B", false);

        // right operand
        testBooleanExpression("-5.0B '= -5.0D", false);
    }

    @Test
    @Order(19)
    public void testTreeWalkInterpreter_EqualityOperators_DoubleComparing_NumberOfDifferentValue_ButDifferentType() {
        // ------
        // Equals
        // ------
        // left operand
        testBooleanExpression("1.0D = 2.0B", false);

        // right operand
        testBooleanExpression("-5.0B = 1.0D", false);

        // ----------
        // Not Equals
        // ----------
        // left operand
        testBooleanExpression("1.0D '= 2.0B", true);

        // right operand
        testBooleanExpression("-5.0B '= 1.0D", true);
    }

    @Test
    @Order(20)
    public void testTreeWalkInterpreter_EqualityOperators_BooleanValues() {
        // Equals
        testBooleanExpression("true = true", true);
        testBooleanExpression("true = false", false);
        testBooleanExpression("false = true", false);
        testBooleanExpression("false = false", true);

        // Not Equals
        testBooleanExpression("true '= true", false);
        testBooleanExpression("true '= false", true);
        testBooleanExpression("false '= true", true);
        testBooleanExpression("false '= false", false);
    }

    @Test
    @Order(21)
    public void testTreeWalkInterpreter_EqualityOperators_DifferentTypes() {
        testBooleanExpression("3 = true", false);
        testBooleanExpression("3 '= true", true);
        testBooleanExpression("false = -7.8F", false);
        testBooleanExpression("false '= -7.8F", true);
    }
}
