package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;
import java.util.function.Predicate;

import static org.junit.jupiter.api.Assertions.fail;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_EqualityOperators extends TreeWalkInterpreterPrintTest_Base {

    // --------------------
    // Numeric comparisons.
    // --------------------

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

    // --------------------------
    // Boolean value comparisons.
    // --------------------------

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

    // -------------------------
    // Null literal comparisons.
    // -------------------------

    /**
     * The null keyword should be equal to any other null value.
     */
    @Test
    @Order(22)
    public void testTreeWalkInterpreter_EqualityOperators_NullKeyword() {
        // Equals
        testBooleanExpression("null = null", true);
        testBooleanExpression("_ = null", true);
        testBooleanExpression("__ = null", true);
        testBooleanExpression("___ = null", true);
        testBooleanExpression("____ = null", true);
        testBooleanExpression("_____ = null", true);
        testBooleanExpression("__[1]__ = null", true);
        testBooleanExpression("__[0]__ = null", true);
        testBooleanExpression("__[-1]__ = null", true);
        testBooleanExpression("__[1024]__ = null", true);

        testBooleanExpression("null = _", true);
        testBooleanExpression("null = __", true);
        testBooleanExpression("null = ___", true);
        testBooleanExpression("null = ____", true);
        testBooleanExpression("null = _____", true);
        testBooleanExpression("null = __[1]__", true);
        testBooleanExpression("null = __[0]__", true);
        testBooleanExpression("null = __[-1]__", true);
        testBooleanExpression("null = __[1024]__", true);

        // Not Equals
        testBooleanExpression("null '= null", false);
        testBooleanExpression("_ '= null", false);
        testBooleanExpression("__ '= null", false);
        testBooleanExpression("___ '= null", false);
        testBooleanExpression("____ '= null", false);
        testBooleanExpression("_____ '= null", false);
        testBooleanExpression("__[1]__ '= null", false);
        testBooleanExpression("__[0]__ '= null", false);
        testBooleanExpression("__[-1]__ '= null", false);
        testBooleanExpression("__[1024]__ '= null", false);

        testBooleanExpression("null '= _", false);
        testBooleanExpression("null '= __", false);
        testBooleanExpression("null '= ___", false);
        testBooleanExpression("null '= ____", false);
        testBooleanExpression("null '= _____", false);
        testBooleanExpression("null '= __[1]__", false);
        testBooleanExpression("null '= __[0]__", false);
        testBooleanExpression("null '= __[-1]__", false);
        testBooleanExpression("null '= __[1024]__", false);
    }

    @Test
    @Order(23)
    public void testTreeWalkInterpreter_EqualityOperators_NullKeyword_AgainstOtherLiteralTypes() {
        // Equals
        testBooleanExpression("true = null", false);
        testBooleanExpression("false = null", false);
        testBooleanExpression("1 = null", false);
        testBooleanExpression("2L = null", false);
        testBooleanExpression("3B = null", false);
        testBooleanExpression("4.0F = null", false);
        testBooleanExpression("5.0D = null", false);
        testBooleanExpression("6.0B = null", false);

        testBooleanExpression("null = true", false);
        testBooleanExpression("null = false", false);
        testBooleanExpression("null = 1", false);
        testBooleanExpression("null = 2L", false);
        testBooleanExpression("null = 3B", false);
        testBooleanExpression("null = 4.0F", false);
        testBooleanExpression("null = 5.0D", false);
        testBooleanExpression("null = 6.0B", false);

        // Not Equals
        testBooleanExpression("true '= null", true);
        testBooleanExpression("false '= null", true);
        testBooleanExpression("1 '= null", true);
        testBooleanExpression("2L '= null", true);
        testBooleanExpression("3B '= null", true);
        testBooleanExpression("4.0F '= null", true);
        testBooleanExpression("5.0D '= null", true);
        testBooleanExpression("6.0B '= null", true);

        testBooleanExpression("null '= true", true);
        testBooleanExpression("null '= false", true);
        testBooleanExpression("null '= 1", true);
        testBooleanExpression("null '= 2L", true);
        testBooleanExpression("null '= 3B", true);
        testBooleanExpression("null '= 4.0F", true);
        testBooleanExpression("null '= 5.0D", true);
        testBooleanExpression("null '= 6.0B", true);
    }

    @Test
    @Order(24)
    public void testTreeWalkInterpreter_EqualityOperators_NullLiterals_SameLength_SameRepresentation() {
        // Equals
        testBooleanExpression("__[-7]__ = __[-7]__", true);
        testBooleanExpression("__[-1]__ = __[-1]__", true);
        testBooleanExpression("__[0]__ = __[0]__", true);
        testBooleanExpression("__[1]__ = __[1]__", true);
        testBooleanExpression("__[14]__ = __[14]__", true);
        testBooleanExpression("_ = _", true);
        testBooleanExpression("__ = __", true);
        testBooleanExpression("___ = ___", true);
        testBooleanExpression("____ = ____", true);
        testBooleanExpression("_____ = _____", true);

        // Not Equals
        testBooleanExpression("__[-7]__ '= __[-7]__", false);
        testBooleanExpression("__[-1]__ '= __[-1]__", false);
        testBooleanExpression("__[0]__ '= __[0]__", false);
        testBooleanExpression("__[1]__ '= __[1]__", false);
        testBooleanExpression("__[14]__ '= __[14]__", false);
        testBooleanExpression("_ '= _", false);
        testBooleanExpression("__ '= __", false);
        testBooleanExpression("___ '= ___", false);
        testBooleanExpression("____ '= ____", false);
        testBooleanExpression("_____ '= _____", false);
    }

    @Test
    @Order(25)
    public void testTreeWalkInterpreter_EqualityOperators_NullLiterals_SameLength_DifferentRepresentation() {
        // Equals
        testBooleanExpression("_[-7]_ = __[-7]__", true);
        testBooleanExpression("_[-1]_ = __[-1]__", true);
        testBooleanExpression("_[0]_ = __[0]__", true);
        testBooleanExpression("_[1]_ = __[1]__", true);
        testBooleanExpression("_[14]_ = __[14]__", true);

        testBooleanExpression("_[1]_ = _[1]__", true);
        testBooleanExpression("_[1]_ = __[1]_", true);
        testBooleanExpression("_[1]_ = __[1]___", true);
        testBooleanExpression("_[1]_ = ___[1]__", true);
        testBooleanExpression("_[1]_ = ___[1]___", true);

        testBooleanExpression("__[1]__ = _[1]__", true);
        testBooleanExpression("__[1]__ = __[1]_", true);
        testBooleanExpression("__[1]__ = __[1]___", true);
        testBooleanExpression("__[1]__ = ___[1]__", true);
        testBooleanExpression("__[1]__ = ___[1]___", true);

        testBooleanExpression("__[1]__ = _", true);
        testBooleanExpression("__[2]__ = __", true);
        testBooleanExpression("__[3]__ = ___", true);
        testBooleanExpression("__[4]__ = ____", true);
        testBooleanExpression("__[5]__ = _____", true);

        testBooleanExpression("__[6]__ = ______", true);
        testBooleanExpression("__[7]__ = _______", true);
        testBooleanExpression("__[8]__ = ________", true);
        testBooleanExpression("__[9]__ = _________", true);
        testBooleanExpression("__[10]__ = __________", true);

        // Not Equals
        testBooleanExpression("_[-7]__ '= __[-7]__", false);
        testBooleanExpression("_[-1]_ '= __[-1]__", false);
        testBooleanExpression("_[0]_ '= __[0]__", false);
        testBooleanExpression("_[1]_ '= __[1]__", false);
        testBooleanExpression("_[14]_ '= __[14]__", false);

        testBooleanExpression("_[1]_ '= _[1]__", false);
        testBooleanExpression("_[1]_ '= __[1]_", false);
        testBooleanExpression("_[1]_ '= __[1]___", false);
        testBooleanExpression("_[1]_ '= ___[1]__", false);
        testBooleanExpression("_[1]_ '= ___[1]___", false);

        testBooleanExpression("__[1]__ '= _[1]__", false);
        testBooleanExpression("__[1]__ '= __[1]_", false);
        testBooleanExpression("__[1]__ '= __[1]___", false);
        testBooleanExpression("__[1]__ '= ___[1]__", false);
        testBooleanExpression("__[1]__ '= ___[1]___", false);

        testBooleanExpression("__[1]__ '= _", false);
        testBooleanExpression("__[2]__ '= __", false);
        testBooleanExpression("__[3]__ '= ___", false);
        testBooleanExpression("__[4]__ '= ____", false);
        testBooleanExpression("__[5]__ '= _____", false);

        testBooleanExpression("__[6]__ '= ______", false);
        testBooleanExpression("__[7]__ '= _______", false);
        testBooleanExpression("__[8]__ '= ________", false);
        testBooleanExpression("__[9]__ '= _________", false);
        testBooleanExpression("__[10]__ '= __________", false);
    }

    @Test
    @Order(26)
    public void testTreeWalkInterpreter_EqualityOperators_NullLiterals_DifferentLength_DifferentRepresentation() {
        // Equals
        testBooleanExpression("__[1]__ = __", false);
        testBooleanExpression("__[1]__ = ___", false);
        testBooleanExpression("__[1]__ = ____", false);
        testBooleanExpression("__[1]__ = _____", false);
        testBooleanExpression("__[1]__ = ______", false);

        testBooleanExpression("_____ = __[-1]__", false);
        testBooleanExpression("_____ = __[-2]__", false);
        testBooleanExpression("_____ = __[-4]__", false);
        testBooleanExpression("_____ = __[-8]__", false);
        testBooleanExpression("_____ = __[-16]__", false);

        testBooleanExpression("_ = __[0]__", false);
        testBooleanExpression("_ = __[2]__", false);
        testBooleanExpression("_ = __[4]__", false);
        testBooleanExpression("_ = __[8]__", false);
        testBooleanExpression("_ = __[16]__", false);

        // Not Equals
        testBooleanExpression("__[1]__ '= __", true);
        testBooleanExpression("__[1]__ '= ___", true);
        testBooleanExpression("__[1]__ '= ____", true);
        testBooleanExpression("__[1]__ '= _____", true);
        testBooleanExpression("__[1]__ '= ______", true);

        testBooleanExpression("_____ '= __[-1]__", true);
        testBooleanExpression("_____ '= __[-2]__", true);
        testBooleanExpression("_____ '= __[-4]__", true);
        testBooleanExpression("_____ '= __[-8]__", true);
        testBooleanExpression("_____ '= __[-16]__", true);

        testBooleanExpression("_ '= __[0]__", true);
        testBooleanExpression("_ '= __[2]__", true);
        testBooleanExpression("_ '= __[4]__", true);
        testBooleanExpression("_ '= __[8]__", true);
        testBooleanExpression("_ '= __[16]__", true);
    }

    @Test
    @Order(27)
    public void testTreeWalkInterpreter_EqualityOperators_NullLiterals_AgainstOtherLiteralValues() {
        // Equals
        testBooleanExpression("true = __[0]__", false);
        testBooleanExpression("false = __[0]__", false);
        testBooleanExpression("1 = __[0]__", false);
        testBooleanExpression("2L = __[0]__", false);
        testBooleanExpression("3B = __[0]__", false);
        testBooleanExpression("4.0F = __[0]__", false);
        testBooleanExpression("5.0D = __[0]__", false);
        testBooleanExpression("6.0B = __[0]__", false);

        testBooleanExpression("__[0]__ = true", false);
        testBooleanExpression("__[0]__ = false", false);
        testBooleanExpression("__[0]__ = 1", false);
        testBooleanExpression("__[0]__ = 2L", false);
        testBooleanExpression("__[0]__ = 3B", false);
        testBooleanExpression("__[0]__ = 4.0F", false);
        testBooleanExpression("__[0]__ = 5.0D", false);
        testBooleanExpression("__[0]__ = 6.0B", false);

        testBooleanExpression("true = _", false);
        testBooleanExpression("false = _", false);
        testBooleanExpression("1 = _", false);
        testBooleanExpression("2L = _", false);
        testBooleanExpression("3B = _", false);
        testBooleanExpression("4.0F = _", false);
        testBooleanExpression("5.0D = _", false);
        testBooleanExpression("6.0B = _", false);

        testBooleanExpression("_ = true", false);
        testBooleanExpression("_ = false", false);
        testBooleanExpression("_ = 1", false);
        testBooleanExpression("_ = 2L", false);
        testBooleanExpression("_ = 3B", false);
        testBooleanExpression("_ = 4.0F", false);
        testBooleanExpression("_ = 5.0D", false);
        testBooleanExpression("_ = 6.0B", false);

        // Not Equals
        testBooleanExpression("true '= __[0]__", true);
        testBooleanExpression("false '= __[0]__", true);
        testBooleanExpression("1 '= __[0]__", true);
        testBooleanExpression("2L '= __[0]__", true);
        testBooleanExpression("3B '= __[0]__", true);
        testBooleanExpression("4.0F '= __[0]__", true);
        testBooleanExpression("5.0D '= __[0]__", true);
        testBooleanExpression("6.0B '= __[0]__", true);

        testBooleanExpression("__[0]__ '= true", true);
        testBooleanExpression("__[0]__ '= false", true);
        testBooleanExpression("__[0]__ '= 1", true);
        testBooleanExpression("__[0]__ '= 2L", true);
        testBooleanExpression("__[0]__ '= 3B", true);
        testBooleanExpression("__[0]__ '= 4.0F", true);
        testBooleanExpression("__[0]__ '= 5.0D", true);
        testBooleanExpression("__[0]__ '= 6.0B", true);

        testBooleanExpression("true '= _", true);
        testBooleanExpression("false '= _", true);
        testBooleanExpression("1 '= _", true);
        testBooleanExpression("2L '= _", true);
        testBooleanExpression("3B '= _", true);
        testBooleanExpression("4.0F '= _", true);
        testBooleanExpression("5.0D '= _", true);
        testBooleanExpression("6.0B '= _", true);

        testBooleanExpression("_ '= true", true);
        testBooleanExpression("_ '= false", true);
        testBooleanExpression("_ '= 1", true);
        testBooleanExpression("_ '= 2L", true);
        testBooleanExpression("_ '= 3B", true);
        testBooleanExpression("_ '= 4.0F", true);
        testBooleanExpression("_ '= 5.0D", true);
        testBooleanExpression("_ '= 6.0B", true);
    }

    // --------------------------
    // Null variable comparisons.
    // --------------------------

    private void testVariableBooleanExpression(String sourceString, boolean expectedValue) {
        lexParseAndInterpret(sourceString);

        String expectedOutput = String.valueOf(expectedValue);
        testOutput(expectedOutput);
    }

    @Test
    @Order(28)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullLiteralValues_SameLength() {
        // Comparisons against null keyword.
        testVariableBooleanExpression("foo << null, :foo = null", true);
        testVariableBooleanExpression("foo << _____, :foo = null", true);
        testVariableBooleanExpression("foo << _[0]_, :foo = null", true);

        testVariableBooleanExpression("foo << null, :foo '= null", false);
        testVariableBooleanExpression("foo << _____, :foo '= null", false);
        testVariableBooleanExpression("foo << _[0]_, :foo '= null", false);

        // Comparisons against null literal values (of same value).
        testVariableBooleanExpression("foo << null, :foo = _[0]_", true);
        testVariableBooleanExpression("foo << _____, :foo = _____", true);
        testVariableBooleanExpression("foo << _____, :foo = _[5]_", true);
        testVariableBooleanExpression("foo << _[0]_, :foo = _[0]_", true);
        testVariableBooleanExpression("foo << _[5]_, :foo = _____", true);

        testVariableBooleanExpression("foo << null, :foo '= _[0]_", false);
        testVariableBooleanExpression("foo << _____, :foo '= _____", false);
        testVariableBooleanExpression("foo << _____, :foo '= _[5]_", false);
        testVariableBooleanExpression("foo << _[0]_, :foo '= _[0]_", false);
        testVariableBooleanExpression("foo << _[5]_, :foo '= _____", false);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Comparisons against null keyword.
        testVariableBooleanExpression("foo << null, :null = foo", true);
        testVariableBooleanExpression("foo << _____, :null = foo", true);
        testVariableBooleanExpression("foo << _[0]_, :null = foo", true);

        testVariableBooleanExpression("foo << null, :null '= foo", false);
        testVariableBooleanExpression("foo << _____, :null '= foo", false);
        testVariableBooleanExpression("foo << _[0]_, :null '= foo", false);

        // Comparisons against null literal values (of same value).
        testVariableBooleanExpression("foo << null, :_[0]_ = foo", true);
        testVariableBooleanExpression("foo << _____, :_____ = foo", true);
        testVariableBooleanExpression("foo << _____, :_[5]_ = foo", true);
        testVariableBooleanExpression("foo << _[0]_, :_[0]_ = foo", true);
        testVariableBooleanExpression("foo << _[5]_, :_____ = foo", true);

        testVariableBooleanExpression("foo << null, :_[0]_ '= foo", false);
        testVariableBooleanExpression("foo << _____, :_____ '= foo", false);
        testVariableBooleanExpression("foo << _____, :_[5]_ '= foo", false);
        testVariableBooleanExpression("foo << _[0]_, :_[0]_ '= foo", false);
        testVariableBooleanExpression("foo << _[5]_, :_____ '= foo", false);
    }

    @Test
    @Order(29)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullLiteralValues_DifferentLength() {
        // Equals operator.
        testVariableBooleanExpression("foo << null, :foo = _____", false);
        testVariableBooleanExpression("foo << null, :foo = _[1]_", false);
        testVariableBooleanExpression("foo << ____, :foo = _[1]_", false);
        testVariableBooleanExpression("foo << ____, :foo = _____", false);
        testVariableBooleanExpression("foo << _[0]_, :foo = ____", false);
        testVariableBooleanExpression("foo << _[1]_, :foo = _[2]_", false);

        // Not equals operator.
        testVariableBooleanExpression("foo << null, :foo '= _____", true);
        testVariableBooleanExpression("foo << null, :foo '= _[1]_", true);
        testVariableBooleanExpression("foo << ____, :foo '= _[1]_", true);
        testVariableBooleanExpression("foo << ____, :foo '= _____", true);
        testVariableBooleanExpression("foo << _[0]_, :foo '= ____", true);
        testVariableBooleanExpression("foo << _[1]_, :foo '= _[2]_", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo << null, :_____ = foo", false);
        testVariableBooleanExpression("foo << null, :_[1]_ = foo", false);
        testVariableBooleanExpression("foo << ____, :_[1]_ = foo", false);
        testVariableBooleanExpression("foo << ____, :_____ = foo", false);
        testVariableBooleanExpression("foo << _[0]_, :____ = foo", false);
        testVariableBooleanExpression("foo << _[1]_, :_[2]_ = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo << null, :_____ '= foo", true);
        testVariableBooleanExpression("foo << null, :_[1]_ '= foo", true);
        testVariableBooleanExpression("foo << ____, :_[1]_ '= foo", true);
        testVariableBooleanExpression("foo << ____, :_____ '= foo", true);
        testVariableBooleanExpression("foo << _[0]_, :____ '= foo", true);
        testVariableBooleanExpression("foo << _[1]_, :_[2]_ '= foo", true);
    }

    @Test
    @Order(30)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNonNullLiteralValues_SameTypes() {
        // Equals operator.
        testVariableBooleanExpression("foo:Integer << null, :foo = 1", false);
        testVariableBooleanExpression("foo:Long << null, :foo = 2L", false);
        testVariableBooleanExpression("foo:BigInteger << null, :foo = 3B", false);
        testVariableBooleanExpression("foo:Float << null, :foo = 4.0F", false);
        testVariableBooleanExpression("foo:Double << null, :foo = 5.0D", false);
        testVariableBooleanExpression("foo:BigDecimal << null, :foo = 6.0B", false);
        testVariableBooleanExpression("foo:Boolean << null, :foo = true", false);
        testVariableBooleanExpression("foo:Boolean << null, :foo = false", false);

        testVariableBooleanExpression("foo:Integer << _____, :foo = 1", false);
        testVariableBooleanExpression("foo:Long << _____, :foo = 2L", false);
        testVariableBooleanExpression("foo:BigInteger << _____, :foo = 3B", false);
        testVariableBooleanExpression("foo:Float << _____, :foo = 4.0F", false);
        testVariableBooleanExpression("foo:Double << _____, :foo = 5.0D", false);
        testVariableBooleanExpression("foo:BigDecimal << _____, :foo = 6.0B", false);
        testVariableBooleanExpression("foo:Boolean << _____, :foo = true", false);
        testVariableBooleanExpression("foo:Boolean << _____, :foo = false", false);

        testVariableBooleanExpression("foo:Integer << _[1]_, :foo = 1", false);
        testVariableBooleanExpression("foo:Long << _[1]_, :foo = 2L", false);
        testVariableBooleanExpression("foo:BigInteger << _[1]_, :foo = 3B", false);
        testVariableBooleanExpression("foo:Float << _[1]_, :foo = 4.0F", false);
        testVariableBooleanExpression("foo:Double << _[1]_, :foo = 5.0D", false);
        testVariableBooleanExpression("foo:BigDecimal << _[1]_, :foo = 6.0B", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :foo = true", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :foo = false", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Integer << null, :foo '= 1", true);
        testVariableBooleanExpression("foo:Long << null, :foo '= 2L", true);
        testVariableBooleanExpression("foo:BigInteger << null, :foo '= 3B", true);
        testVariableBooleanExpression("foo:Float << null, :foo '= 4.0F", true);
        testVariableBooleanExpression("foo:Double << null, :foo '= 5.0D", true);
        testVariableBooleanExpression("foo:BigDecimal << null, :foo '= 6.0B", true);
        testVariableBooleanExpression("foo:Boolean << null, :foo '= true", true);
        testVariableBooleanExpression("foo:Boolean << null, :foo '= false", true);

        testVariableBooleanExpression("foo:Integer << _____, :foo '= 1", true);
        testVariableBooleanExpression("foo:Long << _____, :foo '= 2L", true);
        testVariableBooleanExpression("foo:BigInteger << _____, :foo '= 3B", true);
        testVariableBooleanExpression("foo:Float << _____, :foo '= 4.0F", true);
        testVariableBooleanExpression("foo:Double << _____, :foo '= 5.0D", true);
        testVariableBooleanExpression("foo:BigDecimal << _____, :foo '= 6.0B", true);
        testVariableBooleanExpression("foo:Boolean << _____, :foo '= true", true);
        testVariableBooleanExpression("foo:Boolean << _____, :foo '= false", true);

        testVariableBooleanExpression("foo:Integer << _[1]_, :foo '= 1", true);
        testVariableBooleanExpression("foo:Long << _[1]_, :foo '= 2L", true);
        testVariableBooleanExpression("foo:BigInteger << _[1]_, :foo '= 3B", true);
        testVariableBooleanExpression("foo:Float << _[1]_, :foo '= 4.0F", true);
        testVariableBooleanExpression("foo:Double << _[1]_, :foo '= 5.0D", true);
        testVariableBooleanExpression("foo:BigDecimal << _[1]_, :foo '= 6.0B", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :foo '= true", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :foo '= false", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Integer << null, :1 = foo", false);
        testVariableBooleanExpression("foo:Long << null, :2L = foo", false);
        testVariableBooleanExpression("foo:BigInteger << null, :3B = foo", false);
        testVariableBooleanExpression("foo:Float << null, :4.0F = foo", false);
        testVariableBooleanExpression("foo:Double << null, :5.0D = foo", false);
        testVariableBooleanExpression("foo:BigDecimal << null, :6.0B = foo", false);
        testVariableBooleanExpression("foo:Boolean << null, :true = foo", false);
        testVariableBooleanExpression("foo:Boolean << null, :false = foo", false);

        testVariableBooleanExpression("foo:Integer << _____, :1 = foo", false);
        testVariableBooleanExpression("foo:Long << _____, :2L = foo", false);
        testVariableBooleanExpression("foo:BigInteger << _____, :3B = foo", false);
        testVariableBooleanExpression("foo:Float << _____, :4.0F = foo", false);
        testVariableBooleanExpression("foo:Double << _____, :5.0D = foo", false);
        testVariableBooleanExpression("foo:BigDecimal << _____, :6.0B = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, :true = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, :false = foo", false);

        testVariableBooleanExpression("foo:Integer << _[1]_, :1 = foo", false);
        testVariableBooleanExpression("foo:Long << _[1]_, :2L = foo", false);
        testVariableBooleanExpression("foo:BigInteger << _[1]_, :3B = foo", false);
        testVariableBooleanExpression("foo:Float << _[1]_, :4.0F = foo", false);
        testVariableBooleanExpression("foo:Double << _[1]_, :5.0D = foo", false);
        testVariableBooleanExpression("foo:BigDecimal << _[1]_, :6.0B = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :true = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :false = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Integer << null, :1 '= foo", true);
        testVariableBooleanExpression("foo:Long << null, :2L '= foo", true);
        testVariableBooleanExpression("foo:BigInteger << null, :3B '= foo", true);
        testVariableBooleanExpression("foo:Float << null, :4.0F '= foo", true);
        testVariableBooleanExpression("foo:Double << null, :5.0D '= foo", true);
        testVariableBooleanExpression("foo:BigDecimal << null, :6.0B '= foo", true);
        testVariableBooleanExpression("foo:Boolean << null, :true '= foo", true);
        testVariableBooleanExpression("foo:Boolean << null, :false '= foo", true);

        testVariableBooleanExpression("foo:Integer << _____, :1 '= foo", true);
        testVariableBooleanExpression("foo:Long << _____, :2L '= foo", true);
        testVariableBooleanExpression("foo:BigInteger << _____, :3B '= foo", true);
        testVariableBooleanExpression("foo:Float << _____, :4.0F '= foo", true);
        testVariableBooleanExpression("foo:Double << _____, :5.0D '= foo", true);
        testVariableBooleanExpression("foo:BigDecimal << _____, :6.0B '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, :true '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, :false '= foo", true);

        testVariableBooleanExpression("foo:Integer << _[1]_, :1 '= foo", true);
        testVariableBooleanExpression("foo:Long << _[1]_, :2L '= foo", true);
        testVariableBooleanExpression("foo:BigInteger << _[1]_, :3B '= foo", true);
        testVariableBooleanExpression("foo:Float << _[1]_, :4.0F '= foo", true);
        testVariableBooleanExpression("foo:Double << _[1]_, :5.0D '= foo", true);
        testVariableBooleanExpression("foo:BigDecimal << _[1]_, :6.0B '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :true '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, :false '= foo", true);
    }

    @Test
    @Order(31)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNonNullLiteralValues_DifferentTypes_WithInheritance() {
        // Equals operator.
        testVariableBooleanExpression("foo:Value << null, :foo = 1", false);
        testVariableBooleanExpression("foo:Value << null, :foo = 2L", false);
        testVariableBooleanExpression("foo:Value << null, :foo = 3B", false);
        testVariableBooleanExpression("foo:Value << null, :foo = 4.0F", false);
        testVariableBooleanExpression("foo:Value << null, :foo = 5.0D", false);
        testVariableBooleanExpression("foo:Value << null, :foo = 6.0B", false);
        testVariableBooleanExpression("foo:Value << null, :foo = true", false);
        testVariableBooleanExpression("foo:Value << null, :foo = false", false);

        testVariableBooleanExpression("foo:Value << _____, :foo = 1", false);
        testVariableBooleanExpression("foo:Value << _____, :foo = 2L", false);
        testVariableBooleanExpression("foo:Value << _____, :foo = 3B", false);
        testVariableBooleanExpression("foo:Value << _____, :foo = 4.0F", false);
        testVariableBooleanExpression("foo:Value << _____, :foo = 5.0D", false);
        testVariableBooleanExpression("foo:Value << _____, :foo = 6.0B", false);
        testVariableBooleanExpression("foo:Value << _____, :foo = true", false);
        testVariableBooleanExpression("foo:Value << _____, :foo = false", false);

        testVariableBooleanExpression("foo:Value << _[1]_, :foo = 1", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo = 2L", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo = 3B", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo = 4.0F", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo = 5.0D", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo = 6.0B", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo = true", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo = false", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Value << null, :foo '= 1", true);
        testVariableBooleanExpression("foo:Value << null, :foo '= 2L", true);
        testVariableBooleanExpression("foo:Value << null, :foo '= 3B", true);
        testVariableBooleanExpression("foo:Value << null, :foo '= 4.0F", true);
        testVariableBooleanExpression("foo:Value << null, :foo '= 5.0D", true);
        testVariableBooleanExpression("foo:Value << null, :foo '= 6.0B", true);
        testVariableBooleanExpression("foo:Value << null, :foo '= true", true);
        testVariableBooleanExpression("foo:Value << null, :foo '= false", true);

        testVariableBooleanExpression("foo:Value << _____, :foo '= 1", true);
        testVariableBooleanExpression("foo:Value << _____, :foo '= 2L", true);
        testVariableBooleanExpression("foo:Value << _____, :foo '= 3B", true);
        testVariableBooleanExpression("foo:Value << _____, :foo '= 4.0F", true);
        testVariableBooleanExpression("foo:Value << _____, :foo '= 5.0D", true);
        testVariableBooleanExpression("foo:Value << _____, :foo '= 6.0B", true);
        testVariableBooleanExpression("foo:Value << _____, :foo '= true", true);
        testVariableBooleanExpression("foo:Value << _____, :foo '= false", true);

        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= 1", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= 2L", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= 3B", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= 4.0F", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= 5.0D", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= 6.0B", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= true", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :foo '= false", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Value << null, :1 = foo", false);
        testVariableBooleanExpression("foo:Value << null, :2L = foo", false);
        testVariableBooleanExpression("foo:Value << null, :3B = foo", false);
        testVariableBooleanExpression("foo:Value << null, :4.0F = foo", false);
        testVariableBooleanExpression("foo:Value << null, :5.0D = foo", false);
        testVariableBooleanExpression("foo:Value << null, :6.0B = foo", false);
        testVariableBooleanExpression("foo:Value << null, :true = foo", false);
        testVariableBooleanExpression("foo:Value << null, :false = foo", false);

        testVariableBooleanExpression("foo:Value << _____, :1 = foo", false);
        testVariableBooleanExpression("foo:Value << _____, :2L = foo", false);
        testVariableBooleanExpression("foo:Value << _____, :3B = foo", false);
        testVariableBooleanExpression("foo:Value << _____, :4.0F = foo", false);
        testVariableBooleanExpression("foo:Value << _____, :5.0D = foo", false);
        testVariableBooleanExpression("foo:Value << _____, :6.0B = foo", false);
        testVariableBooleanExpression("foo:Value << _____, :true = foo", false);
        testVariableBooleanExpression("foo:Value << _____, :false = foo", false);

        testVariableBooleanExpression("foo:Value << _[1]_, :1 = foo", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :2L = foo", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :3B = foo", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :4.0F = foo", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :5.0D = foo", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :6.0B = foo", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :true = foo", false);
        testVariableBooleanExpression("foo:Value << _[1]_, :false = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Value << null, :1 '= foo", true);
        testVariableBooleanExpression("foo:Value << null, :2L '= foo", true);
        testVariableBooleanExpression("foo:Value << null, :3B '= foo", true);
        testVariableBooleanExpression("foo:Value << null, :4.0F '= foo", true);
        testVariableBooleanExpression("foo:Value << null, :5.0D '= foo", true);
        testVariableBooleanExpression("foo:Value << null, :6.0B '= foo", true);
        testVariableBooleanExpression("foo:Value << null, :true '= foo", true);
        testVariableBooleanExpression("foo:Value << null, :false '= foo", true);

        testVariableBooleanExpression("foo:Value << _____, :1 '= foo", true);
        testVariableBooleanExpression("foo:Value << _____, :2L '= foo", true);
        testVariableBooleanExpression("foo:Value << _____, :3B '= foo", true);
        testVariableBooleanExpression("foo:Value << _____, :4.0F '= foo", true);
        testVariableBooleanExpression("foo:Value << _____, :5.0D '= foo", true);
        testVariableBooleanExpression("foo:Value << _____, :6.0B '= foo", true);
        testVariableBooleanExpression("foo:Value << _____, :true '= foo", true);
        testVariableBooleanExpression("foo:Value << _____, :false '= foo", true);

        testVariableBooleanExpression("foo:Value << _[1]_, :1 '= foo", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :2L '= foo", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :3B '= foo", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :4.0F '= foo", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :5.0D '= foo", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :6.0B '= foo", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :true '= foo", true);
        testVariableBooleanExpression("foo:Value << _[1]_, :false '= foo", true);
    }

    @Test
    @Order(32)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNonNullLiteralValues_DifferentTypes_WithoutInheritance() {
        // Equals operator.
        testVariableBooleanExpression("foo:Integer << null, :foo = 1L", false);
        testVariableBooleanExpression("foo:Integer << null, :foo = 2B", false);
        testVariableBooleanExpression("foo:Integer << null, :foo = 3.0F", false);
        testVariableBooleanExpression("foo:Integer << null, :foo = 4.0D", false);
        testVariableBooleanExpression("foo:Integer << null, :foo = 5.0B", false);
        testVariableBooleanExpression("foo:Integer << null, :foo = true", false);
        testVariableBooleanExpression("foo:Integer << null, :foo = false", false);

        testVariableBooleanExpression("foo:Integer << _____, :foo = 2B", false);
        testVariableBooleanExpression("foo:Integer << _____, :foo = 3.0F", false);
        testVariableBooleanExpression("foo:Integer << _____, :foo = 4.0D", false);
        testVariableBooleanExpression("foo:Integer << _____, :foo = 5.0B", false);
        testVariableBooleanExpression("foo:Integer << _____, :foo = true", false);
        testVariableBooleanExpression("foo:Integer << _____, :foo = false", false);

        testVariableBooleanExpression("foo:Integer << _[1]_, :foo = 2B", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo = 3.0F", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo = 4.0D", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo = 5.0B", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo = true", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo = false", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Integer << null, :foo '= 2B", true);
        testVariableBooleanExpression("foo:Integer << null, :foo '= 3.0F", true);
        testVariableBooleanExpression("foo:Integer << null, :foo '= 4.0D", true);
        testVariableBooleanExpression("foo:Integer << null, :foo '= 5.0B", true);
        testVariableBooleanExpression("foo:Integer << null, :foo '= true", true);
        testVariableBooleanExpression("foo:Integer << null, :foo '= false", true);

        testVariableBooleanExpression("foo:Integer << _____, :foo '= 2B", true);
        testVariableBooleanExpression("foo:Integer << _____, :foo '= 3.0F", true);
        testVariableBooleanExpression("foo:Integer << _____, :foo '= 4.0D", true);
        testVariableBooleanExpression("foo:Integer << _____, :foo '= 5.0B", true);
        testVariableBooleanExpression("foo:Integer << _____, :foo '= true", true);
        testVariableBooleanExpression("foo:Integer << _____, :foo '= false", true);

        testVariableBooleanExpression("foo:Integer << _[1]_, :foo '= 2B", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo '= 3.0F", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo '= 4.0D", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo '= 5.0B", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo '= true", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :foo '= false", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Integer << null, :1L = foo", false);
        testVariableBooleanExpression("foo:Integer << null, :2B = foo", false);
        testVariableBooleanExpression("foo:Integer << null, :3.0F = foo", false);
        testVariableBooleanExpression("foo:Integer << null, :4.0D = foo", false);
        testVariableBooleanExpression("foo:Integer << null, :5.0B = foo", false);
        testVariableBooleanExpression("foo:Integer << null, :true = foo", false);
        testVariableBooleanExpression("foo:Integer << null, :false = foo", false);

        testVariableBooleanExpression("foo:Integer << _____, :1L = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, :2B = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, :3.0F = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, :4.0D = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, :5.0B = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, :true = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, :false = foo", false);

        testVariableBooleanExpression("foo:Integer << _[1]_, :1L = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :2B = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :3.0F = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :4.0D = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :5.0B = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :true = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, :false = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Integer << null, :1L '= foo", true);
        testVariableBooleanExpression("foo:Integer << null, :2B '= foo", true);
        testVariableBooleanExpression("foo:Integer << null, :3.0F '= foo", true);
        testVariableBooleanExpression("foo:Integer << null, :4.0D '= foo", true);
        testVariableBooleanExpression("foo:Integer << null, :5.0B '= foo", true);
        testVariableBooleanExpression("foo:Integer << null, :true '= foo", true);
        testVariableBooleanExpression("foo:Integer << null, :false '= foo", true);

        testVariableBooleanExpression("foo:Integer << _____, :1L '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, :2B '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, :3.0F '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, :4.0D '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, :5.0B '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, :true '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, :false '= foo", true);

        testVariableBooleanExpression("foo:Integer << _[1]_, :1L '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :2B '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :3.0F '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :4.0D '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :5.0B '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :true '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, :false '= foo", true);
    }

    @Test
    @Order(33)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullVariable_OfSameType_SameLength() {
        testVariableBooleanExpression("foo << null, bar << null, :foo = bar", true);
        testVariableBooleanExpression("foo << null, bar << _[0]_, :foo = bar", true);
        testVariableBooleanExpression("foo << _[0]_, bar << null, :foo = bar", true);
        testVariableBooleanExpression("foo << _____, bar << _____, :foo = bar", true);
        testVariableBooleanExpression("foo << _[5]_, bar << _[5]_, :foo = bar", true);
        testVariableBooleanExpression("foo << _____, bar << _[5]_, :foo = bar", true);
        testVariableBooleanExpression("foo << _[5]_, bar << _____, :foo = bar", true);

        testVariableBooleanExpression("foo << null, bar << null, :foo '= bar", false);
        testVariableBooleanExpression("foo << null, bar << _[0]_, :foo '= bar", false);
        testVariableBooleanExpression("foo << _[0]_, bar << null, :foo '= bar", false);
        testVariableBooleanExpression("foo << _____, bar << _____, :foo '= bar", false);
        testVariableBooleanExpression("foo << _[5]_, bar << _[5]_, :foo '= bar", false);
        testVariableBooleanExpression("foo << _____, bar << _[5]_, :foo '= bar", false);
        testVariableBooleanExpression("foo << _[5]_, bar << _____, :foo '= bar", false);
    }

    @Test
    @Order(34)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullVariable_OfSameType_DifferentLength() {
        testVariableBooleanExpression("foo << null, bar << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo << _[5]_, bar << null, :foo = bar", false);
        testVariableBooleanExpression("foo << null, bar << _____, :foo = bar", false);
        testVariableBooleanExpression("foo << _____, bar << null, :foo = bar", false);
        testVariableBooleanExpression("foo << _[1]_, bar << _____, :foo = bar", false);
        testVariableBooleanExpression("foo << _____, bar << _[1]_, :foo = bar", false);
        testVariableBooleanExpression("foo << _[5]_, bar << _[1]_, :foo = bar", false);
        testVariableBooleanExpression("foo << _[1]_, bar << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo << _____, bar << _, :foo = bar", false);
        testVariableBooleanExpression("foo << _, bar << _____, :foo = bar", false);

        testVariableBooleanExpression("foo << null, bar << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo << _[5]_, bar << null, :foo '= bar", true);
        testVariableBooleanExpression("foo << null, bar << _____, :foo '= bar", true);
        testVariableBooleanExpression("foo << _____, bar << null, :foo '= bar", true);
        testVariableBooleanExpression("foo << _[1]_, bar << _____, :foo '= bar", true);
        testVariableBooleanExpression("foo << _____, bar << _[1]_, :foo '= bar", true);
        testVariableBooleanExpression("foo << _[5]_, bar << _[1]_, :foo '= bar", true);
        testVariableBooleanExpression("foo << _[1]_, bar << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo << _____, bar << _, :foo '= bar", true);
        testVariableBooleanExpression("foo << _, bar << _____, :foo '= bar", true);
    }

    @Test
    @Order(35)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullVariable_OfDifferentType_WithInheritance_SameLength() {
        // Equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << null, :foo = bar", true);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[0]_, :foo = bar", true);
        testVariableBooleanExpression("foo:Number << _[0]_, bar:Integer << null, :foo = bar", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _____, :foo = bar", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[5]_, :foo = bar", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[5]_, :foo = bar", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _____, :foo = bar", true);

        // Not equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << null, :foo '= bar", false);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[0]_, :foo '= bar", false);
        testVariableBooleanExpression("foo:Number << _[0]_, bar:Integer << null, :foo '= bar", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _____, :foo '= bar", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[5]_, :foo '= bar", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[5]_, :foo '= bar", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _____, :foo '= bar", false);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << null, :bar = foo", true);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[0]_, :bar = foo", true);
        testVariableBooleanExpression("foo:Number << _[0]_, bar:Integer << null, :bar = foo", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _____, :bar = foo", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[5]_, :bar = foo", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[5]_, :bar = foo", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _____, :bar = foo", true);

        // Not equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << null, :bar '= foo", false);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[0]_, :bar '= foo", false);
        testVariableBooleanExpression("foo:Number << _[0]_, bar:Integer << null, :bar '= foo", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _____, :bar '= foo", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[5]_, :bar '= foo", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[5]_, :bar '= foo", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _____, :bar '= foo", false);
    }

    @Test
    @Order(36)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullVariable_OfDifferentType_WithInheritance_DifferentLength() {
        // Equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << null, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _____, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << null, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _____, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[1]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[1]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _, :foo = bar", false);
        testVariableBooleanExpression("foo:Number << _, bar:Integer << _____, :foo = bar", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << null, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _____, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << null, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _____, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[1]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[1]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _, bar:Integer << _____, :foo '= bar", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[5]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << null, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _____, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << null, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _____, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[1]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[1]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _[5]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _, bar:Integer << _____, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _[5]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << null, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << null, bar:Integer << _____, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << null, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _____, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _[1]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _[5]_, bar:Integer << _[1]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << _[5]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << _, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _, bar:Integer << _____, :bar '= foo", true);
    }

    @Test
    @Order(37)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullVariable_OfDifferentType_WithoutInheritance_SameLength() {
        // Equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << null, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[0]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _[0]_, bar:Integer << null, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _____, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _____, :foo = bar", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << null, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[0]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[0]_, bar:Integer << null, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _____, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _____, :foo '= bar", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << null, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[0]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[0]_, bar:Integer << null, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _____, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[5]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[5]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _____, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << null, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[0]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[0]_, bar:Integer << null, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _____, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[5]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[5]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _____, :bar '= foo", true);
    }

    @Test
    @Order(38)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullVariable_OfDifferentType_WithoutInheritance_DifferentLength() {
        // Equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << null, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _____, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << null, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _____, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[1]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[1]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _[5]_, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _, :foo = bar", false);
        testVariableBooleanExpression("foo:Boolean << _, bar:Integer << _____, :foo = bar", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << null, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _____, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << null, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _____, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[1]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[1]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _[5]_, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _, bar:Integer << _____, :foo '= bar", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[5]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << null, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _____, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << null, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _____, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[1]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[1]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _[5]_, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _, bar:Integer << _____, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _[5]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << null, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << _____, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << null, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _____, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _[1]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[5]_, bar:Integer << _[1]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << _[5]_, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << _, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _, bar:Integer << _____, :bar '= foo", true);
    }

    /**
     * This method subclasses the TreeWalkInterpreter class to introduce new behavior to alter the instantiatedType of
     * any variables that return true against the given predicate. This is used to produce an error case which wouldn't
     * otherwise exist, but should still be tested against to ensure accuracy of the error report.
     *
     * @param sourceString            The Vikari source code string to execute.
     * @param shouldAlterVariableType The Predicate to test for which variable references to modify the instantiatedType for.
     */
    private void lexParseAndInterpret_WithNullTypeError(String sourceString, Predicate<AtonementCrystal> shouldAlterVariableType) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        // Alter class behavior to override the instantiatedType of variable references that match against the given predicate.
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter() {
            @Override
            public AtonementCrystal visit(VariableExpression expr) {
                AtonementCrystal variable = super.visit(expr);
                if (shouldAlterVariableType.test(variable)) {
                    variable.setInstantiatedType(VikariType.NULL.getTypeCrystal());
                }
                return variable;
            }
        };

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLineFromCache);

        parser.setGlobalAtonementField(globalAtonementField);
        interpreter.setGlobalAtonementField(globalAtonementField);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        if (syntaxErrorReporter.hasErrors()) {
            restorePrintStream();
            syntaxErrorReporter.reportSyntaxErrors();
            fail("Expected no syntax errors for test case.");
        }

        interpreter.interpret(null, parsedStatements);
    }

    /**
     * This test modifies the state of NullCrystals based on their declaredTypes in order to trigger a RuntimeError case
     * for when a NullCrystal does not have an instantiatedType of type NullTypeCrystal. In order to ensure that the
     * error is reported correctly, for all cases.<br/>
     * <br/>
     * As this error case normally should never be triggered by the TreeWalkInterpreter, this class is overridden in
     * order to model this alternate behavior using {@link #lexParseAndInterpret_WithNullTypeError(String, Predicate)
     * lexParseAndInterpret_WithNullTypeError}. (But only for this test case.)
     */
    @Test
    @Order(39)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNullVariable_WithNullTypeError() {
        String sourceString = """
                boolean:Boolean << null
                integer:Integer << null
                :boolean = integer
                """;

        // Predicates for each Type error scenario. (To trigger overriding the instantiatedType of the NullCrystals.)
        Predicate<AtonementCrystal> hasTypeBoolean = crystal -> crystal.getDeclaredType().isEqual(VikariType.BOOLEAN);
        Predicate<AtonementCrystal> hasTypeInteger = crystal -> crystal.getDeclaredType().isEqual(VikariType.INTEGER);
        Predicate<AtonementCrystal> hasEitherType = hasTypeBoolean.or(hasTypeInteger);

        // Left operand has the Type error.
        lexParseAndInterpret_WithNullTypeError(sourceString, hasTypeBoolean);
        testOutput("<repl>:3:2:\n" +
                "    :boolean = integer\n" +
                "     ^\n" +
                "    Left operand of equality expression has type Null, but its TypeCrystal doesn't have type " +
                "NullTypeCrystal.\n");

        // Right operand has the Type error.
        lexParseAndInterpret_WithNullTypeError(sourceString, hasTypeInteger);
        testOutput("<repl>:3:12:\n" +
                "    :boolean = integer\n" +
                "               ^\n" +
                "    Right operand of equality expression has type Null, but its TypeCrystal doesn't have type " +
                "NullTypeCrystal.\n");

        // Left and right operands both have the Type error.
        lexParseAndInterpret_WithNullTypeError(sourceString, hasEitherType);
        testOutput("<repl>:3:10:\n" +
                "    :boolean = integer\n" +
                "             ^\n" +
                "    Left and right operands of equality expression have type Null, but their TypeCrystals don't have " +
                "type NullTypeCrystal.\n");
    }

    @Test
    @Order(40)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNonNullVariable_OfSameType() {
        // Equals operator.
        testVariableBooleanExpression("foo:Integer << null, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Integer << 2, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Integer << null, bar:Integer << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Integer << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Integer << 2, :bar '= foo", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Integer << null, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Integer << 2, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Integer << null, bar:Integer << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Integer << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Integer << 2, :foo '= bar", true);
    }

    @Test
    @Order(41)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNonNullVariable_WithInheritance() {
        // Equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << 2, :bar = foo", false);

        testVariableBooleanExpression("foo:Integer << null, bar:Number << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Number << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Number << 2, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << 2, :bar '= foo", true);

        testVariableBooleanExpression("foo:Integer << null, bar:Number << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Number << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Number << 2, :bar '= foo", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << 2, :bar = foo", false);

        testVariableBooleanExpression("foo:Integer << null, bar:Number << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Number << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Number << 2, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Number << null, bar:Integer << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _[1]_, bar:Integer << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Number << _____, bar:Integer << 2, :foo '= bar", true);

        testVariableBooleanExpression("foo:Integer << null, bar:Number << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Number << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Number << 2, :foo '= bar", true);
    }

    @Test
    @Order(42)
    public void testTreeWalkInterpreter_EqualityOperators_NullVariable_AgainstNonNullVariable_WithoutInheritance() {
        // Equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << 2, :bar = foo", false);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << true, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << true, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << true, :bar = foo", false);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << false, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << false, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << false, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << 2, :bar '= foo", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << 2, :bar '= foo", true);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << true, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << true, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << true, :bar '= foo", true);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << false, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << false, :bar '= foo", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << false, :bar '= foo", true);

        // -----------------------
        // Swap order of operands.
        // -----------------------

        // Equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << 2, :bar = foo", false);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << 2, :bar = foo", false);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << true, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << true, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << true, :bar = foo", false);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << false, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << false, :bar = foo", false);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << false, :bar = foo", false);

        // Not equals operator.
        testVariableBooleanExpression("foo:Boolean << null, bar:Integer << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _[1]_, bar:Integer << 2, :foo '= bar", true);
        testVariableBooleanExpression("foo:Boolean << _____, bar:Integer << 2, :foo '= bar", true);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << true, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << true, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << true, :foo '= bar", true);

        testVariableBooleanExpression("foo:Integer << null, bar:Boolean << false, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _[1]_, bar:Boolean << false, :foo '= bar", true);
        testVariableBooleanExpression("foo:Integer << _____, bar:Boolean << false, :foo '= bar", true);
    }
}
