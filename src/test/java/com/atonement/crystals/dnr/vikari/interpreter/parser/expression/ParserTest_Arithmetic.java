package com.atonement.crystals.dnr.vikari.interpreter.parser.expression;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.RightDivideOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonement.crystals.dnr.vikari.core.expression.Expression;
import com.atonement.crystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonement.crystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonement.crystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonement.crystals.dnr.vikari.core.statement.Statement;
import com.atonement.crystals.dnr.vikari.error.SyntaxError;
import com.atonement.crystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonement.crystals.dnr.vikari.interpreter.Lexer;
import com.atonement.crystals.dnr.vikari.interpreter.Parser;
import com.atonement.crystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonement.crystals.dnr.vikari.interpreter.TestUtils.assertNoSyntaxErrors;
import static com.atonement.crystals.dnr.vikari.interpreter.TestUtils.testSyntaxError;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Arithmetic {

    @Test
    @Order(1)
    public void testArithmeticOperators_Add() {
        String sourceString = "2 + 7";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) expression;
        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(AddOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((LiteralExpression) left).getValue();
        AtonementCrystal rightOperand = ((LiteralExpression) right).getValue();

        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(IntegerCrystal.class, rightOperand.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber = (IntegerCrystal) leftOperand;
        IntegerCrystal rightNumber = (IntegerCrystal) rightOperand;

        assertEquals(2, leftNumber.getValue(), "Unexpected literal value.");
        assertEquals(7, rightNumber.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(2)
    public void testArithmeticOperators_Subtract() {
        String sourceString = "3 - 8";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) expression;
        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(SubtractOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((LiteralExpression) left).getValue();
        AtonementCrystal rightOperand = ((LiteralExpression) right).getValue();

        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(IntegerCrystal.class, rightOperand.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber = (IntegerCrystal) leftOperand;
        IntegerCrystal rightNumber = (IntegerCrystal) rightOperand;

        assertEquals(3, leftNumber.getValue(), "Unexpected literal value.");
        assertEquals(8, rightNumber.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(3)
    public void testArithmeticOperators_Multiply() {
        String sourceString = "4 * 9";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) expression;
        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(MultiplyOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((LiteralExpression) left).getValue();
        AtonementCrystal rightOperand = ((LiteralExpression) right).getValue();

        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(IntegerCrystal.class, rightOperand.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber = (IntegerCrystal) leftOperand;
        IntegerCrystal rightNumber = (IntegerCrystal) rightOperand;

        assertEquals(4, leftNumber.getValue(), "Unexpected literal value.");
        assertEquals(9, rightNumber.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(4)
    public void testArithmeticOperators_LeftDivide() {
        String sourceString = "5 / 10";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) expression;
        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(LeftDivideOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((LiteralExpression) left).getValue();
        AtonementCrystal rightOperand = ((LiteralExpression) right).getValue();

        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(IntegerCrystal.class, rightOperand.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber = (IntegerCrystal) leftOperand;
        IntegerCrystal rightNumber = (IntegerCrystal) rightOperand;

        assertEquals(5, leftNumber.getValue(), "Unexpected literal value.");
        assertEquals(10, rightNumber.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(5)
    public void testArithmeticOperators_RightDivide() {
        String sourceString = "6 \\ 11";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) expression;
        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(RightDivideOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((LiteralExpression) left).getValue();
        AtonementCrystal rightOperand = ((LiteralExpression) right).getValue();

        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(IntegerCrystal.class, rightOperand.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber = (IntegerCrystal) leftOperand;
        IntegerCrystal rightNumber = (IntegerCrystal) rightOperand;

        assertEquals(6, leftNumber.getValue(), "Unexpected literal value.");
        assertEquals(11, rightNumber.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(6)
    public void testArithmeticOperators_AndGrouping_SingleLine() {
        String sourceString = "2 + [7 - [22 / 3] * 8]";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // 1. first expression (add expression: "2 + [7 - [22 / 3] * 8]")
        Expression expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) expression;
        Expression left1 = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right1 = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left1.getClass(), "Unexpected expression type.");
        assertEquals(AddOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(GroupingExpression.class, right1.getClass(), "Unexpected expression type.");

        // 2. first left operand (literal expression: "2")
        AtonementCrystal leftOperand = ((LiteralExpression) left1).getValue();
        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber = (IntegerCrystal) leftOperand;
        assertEquals(2, leftNumber.getValue(), "Unexpected literal value.");

        // 3. first right operand (grouping expression: "[7 - [22 / 3] * 8]")
        GroupingExpression groupingExpression1 = (GroupingExpression) right1;

        // 4. inner expression (subtract expression: "7 - [22 / 3] * 8")
        Expression innerExpression1 = groupingExpression1.getExpression();
        assertEquals(BinaryExpression.class, innerExpression1.getClass(), "Unexpected expression type.");
        BinaryExpression binaryExpression2 = (BinaryExpression) innerExpression1;

        Expression left2 = binaryExpression2.getLeft();
        BinaryOperatorCrystal operator2 = binaryExpression2.getOperator();
        Expression right2 = binaryExpression2.getRight();

        assertEquals(LiteralExpression.class, left2.getClass(), "Unexpected expression type.");
        assertEquals(SubtractOperatorCrystal.class, operator2.getClass(), "Unexpected operator type.");
        assertEquals(BinaryExpression.class, right2.getClass(), "Unexpected expression type.");

        // 5. second left operand (literal expression: "7")
        AtonementCrystal leftOperand2 = ((LiteralExpression) left2).getValue();
        assertEquals(IntegerCrystal.class, leftOperand2.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber2 = (IntegerCrystal) leftOperand2;
        assertEquals(7, leftNumber2.getValue(), "Unexpected literal value.");

        // 7. second right operand (multiply expression: "[22 / 3] * 8")
        BinaryExpression binaryExpression3 = (BinaryExpression) right2;

        Expression left3 = binaryExpression3.getLeft();
        BinaryOperatorCrystal operator3 = binaryExpression3.getOperator();
        Expression right3 = binaryExpression3.getRight();

        assertEquals(GroupingExpression.class, left3.getClass(), "Unexpected expression type.");
        assertEquals(MultiplyOperatorCrystal.class, operator3.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right3.getClass(), "Unexpected expression type.");

        // 8. third left operand (grouping expression: "[22 / 3]")
        GroupingExpression groupingExpression2 = (GroupingExpression) left3;
        Expression innerExpression2 = groupingExpression2.getExpression();
        assertEquals(BinaryExpression.class, innerExpression2.getClass(), "Unexpected expression type.");

        // 9. third right operand (literal expression: "8")
        AtonementCrystal rightOperand3 = ((LiteralExpression) right3).getValue();
        assertEquals(IntegerCrystal.class, rightOperand3.getClass(), "Unexpected literal type.");

        IntegerCrystal rightNumber3 = (IntegerCrystal) rightOperand3;
        assertEquals(8, rightNumber3.getValue(), "Unexpected literal value.");

        // 10. inner expression (divide expression: "22 / 3")
        BinaryExpression binaryExpression4 = (BinaryExpression) innerExpression2;

        Expression left4 = binaryExpression4.getLeft();
        BinaryOperatorCrystal operator4 = binaryExpression4.getOperator();
        Expression right4 = binaryExpression4.getRight();

        assertEquals(LiteralExpression.class, left4.getClass(), "Unexpected expression type.");
        assertEquals(LeftDivideOperatorCrystal.class, operator4.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right4.getClass(), "Unexpected expression type.");

        // 11. fourth left operand (literal expression: "22")
        AtonementCrystal leftOperand4 = ((LiteralExpression) left4).getValue();
        assertEquals(IntegerCrystal.class, leftOperand4.getClass(), "Unexpected literal type.");

        IntegerCrystal leftNumber4 = (IntegerCrystal) leftOperand4;
        assertEquals(22, leftNumber4.getValue(), "Unexpected literal value.");

        // 12. fourth right operand (literal expression: "3")
        AtonementCrystal rightOperand4 = ((LiteralExpression) right4).getValue();
        assertEquals(IntegerCrystal.class, rightOperand4.getClass(), "Unexpected literal type.");

        IntegerCrystal rightNumber4 = (IntegerCrystal) rightOperand4;
        assertEquals(3, rightNumber4.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(7)
    public void testArithmeticOperators_InvalidOperator() {
        String sourceString = "2 // 7";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        parser.parse(null, lexedStatements);

        assertTrue(syntaxErrorReporter.hasErrors(), "Expected a syntax error for missing opening square bracket.");
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        assertEquals(1, syntaxErrors.size(), "Unexpected number of syntax errors.");

        testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 2), sourceString, "Expected expression.");
    }

    // TODO: Add more error test cases.

    @Test
    @Order(8)
    public void testArithmeticOperators_NoOperands() {
        String sourceString = "+";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        parser.parse(null, lexedStatements);

        assertTrue(syntaxErrorReporter.hasErrors(), "Expected a syntax error for missing opening square bracket.");
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        assertEquals(1, syntaxErrors.size(), "Unexpected number of syntax errors.");

        testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 0), sourceString, "Expected expression.");
    }

    @Test
    @Order(9)
    public void testArithmeticOperators_MissingLeftOperand() {
        String sourceString = "* 5";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        parser.parse(null, lexedStatements);

        assertTrue(syntaxErrorReporter.hasErrors(), "Expected a syntax error for missing opening square bracket.");
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        assertEquals(1, syntaxErrors.size(), "Unexpected number of syntax errors.");

        testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 0), sourceString, "Expected expression.");
    }

    @Test
    @Order(10)
    public void testArithmeticOperators_MissingRightOperand() {
        String sourceString = "2 +";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        parser.parse(null, lexedStatements);

        assertTrue(syntaxErrorReporter.hasErrors(), "Expected a syntax error for missing opening square bracket.");
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        assertEquals(1, syntaxErrors.size(), "Unexpected number of syntax errors.");

        testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 2), sourceString, "Expected expression.");
    }

    @Test
    @Order(11)
    public void testArithmeticOperators_DoubleOperator() {
        String sourceString = "4 // 2";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        parser.parse(null, lexedStatements);

        assertTrue(syntaxErrorReporter.hasErrors(), "Expected a syntax error for missing opening square bracket.");
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        assertEquals(1, syntaxErrors.size(), "Unexpected number of syntax errors.");

        testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 2), sourceString, "Expected expression.");
    }


}
