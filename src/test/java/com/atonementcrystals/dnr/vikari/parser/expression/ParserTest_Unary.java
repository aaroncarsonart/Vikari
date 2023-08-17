package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalAndOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalNotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalOrOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.NegateOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BooleanLogicExpression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Base;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.*;

/**
 * Note that basic negation of numeric literals (i.e.: "-3") is handled by the Lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Unary extends ParserTest_Base {

    @Test
    @Order(1)
    public void testParser_Expression_NegateGrouping() {
        String sourceString = "-[5 - 3]";

        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        // 1. first expression (unary: "-[5 - 3]")
        UnaryExpression unaryExpression = assertUnaryExpression(statements.get(0), location(0, 0));
        testOperator(unaryExpression.getOperator(), NegateOperatorCrystal.class, location(0, 0));

        // 2. unary operand (grouping: "[5 - 3]")
        GroupingExpression groupingExpression = assertGroupingExpression(unaryExpression.getOperand(), location(0, 1));

        // 3. inner expression (subtract: "5 - 3")
        testBinaryExpression(groupingExpression.getExpression(), SubtractOperatorCrystal.class, 5, 3);
    }

    @Test
    @Order(2)
    public void testParser_Expression_Negate_InvalidOperand() {
        String sourceString = "-true";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<Statement> statements = ParserTest_Utils.lexAndParse(sourceString, errorReporter, 1);
        assertStatementCount(statements, 1);

        UnaryExpression unaryExpression = assertUnaryExpression(statements.get(0), location(0, 0));
        testOperator(unaryExpression.getOperator(), NegateOperatorCrystal.class, location(0, 0));
        testLiteralExpression(unaryExpression.getOperand(), BooleanCrystal.class, true, location(0, 1));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Negate expression expects a Number as " +
                "its operand.");
    }

    @Test
    @Order(3)
    public void testParser_Expression_LogicalNot_InvalidOperand() {
        String sourceString = "'3";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<Statement> statements = ParserTest_Utils.lexAndParse(sourceString, errorReporter, 1);
        assertStatementCount(statements, 1);

        UnaryExpression unaryExpression = assertUnaryExpression(statements.get(0), location(0, 0));
        testOperator(unaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 0));
        testLiteralExpression(unaryExpression.getOperand(), IntegerCrystal.class, 3, location(0, 1));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Logical not expression expects a " +
                "Boolean as its operand.");
    }

    @Test
    @Order(4)
    public void testParser_Expression_LogicalNot_BooleanLiteral() {
        String sourceString = "'true";

        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        UnaryExpression unaryExpression = assertUnaryExpression(statements.get(0), location(0, 0));
        testOperator(unaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 0));
        testLiteralExpression(unaryExpression.getOperand(), BooleanCrystal.class, true, location(0, 1));
    }

    @Test
    @Order(5)
    public void testParser_Expression_LogicalNot_BooleanVariable() {
        String sourceString = "foo << false, 'foo";

        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 2);

        // 1. first statement (declaration: "foo")
        VariableDeclarationStatement declaration = assertVariableDeclaration(statements.get(0), location(0, 0));
        testVariableCrystal(declaration.getDeclaredVariable(), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN,
                location(0, 0));
        testOperator(declaration.getAssignmentOperator(), LeftAssignmentOperatorCrystal.class, location (0, 4));
        testLiteralExpression(declaration.getInitializerExpression(), BooleanCrystal.class, false, location(0, 7));

        // 2. second statement (unary: "'foo")
        UnaryExpression unaryExpression = assertUnaryExpression(statements.get(1), location(0, 14));
        testOperator(unaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 14));
        testVariableExpression(unaryExpression.getOperand(), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN,
                location(0, 15));
    }

    @Test
    @Order(6)
    public void testParser_Expression_LogicalNot_OnGroupingExpression() {
        String sourceString = "'[true]";

        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        // 1. first expression (unary: "'[true]")
        UnaryExpression unaryExpression = assertUnaryExpression(statements.get(0), location(0, 0));
        testOperator(unaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 0));

        // 2. unary operand (grouping: "[true]")
        GroupingExpression groupingExpression = assertGroupingExpression(unaryExpression.getOperand(), location(0, 1));
        testLiteralExpression(groupingExpression.getExpression(), BooleanCrystal.class, true, location(0, 2));
    }

    @Test
    @Order(7)
    public void testParser_Expression_LogicalNot_InLogicalExpression() {
        String sourceString = "'true ^ 'false";

        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        // 1. first expression (logical: "'true ^ 'false")
        BooleanLogicExpression logicExpression = assertLogicalExpression(statements.get(0), location(0, 0));
        testOperator(logicExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 6));

        // 2. left operand (unary: "'true")
        UnaryExpression leftUnaryExpression = assertUnaryExpression(logicExpression.getLeft(), location(0, 0));
        testOperator(leftUnaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 0));
        testLiteralExpression(leftUnaryExpression.getOperand(), BooleanCrystal.class, true, location(0, 1));

        // 3. right operand (unary: "'false")
        UnaryExpression rightUnaryExpression = assertUnaryExpression(logicExpression.getRight(), location(0, 8));
        testOperator(rightUnaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 8));
        testLiteralExpression(rightUnaryExpression.getOperand(), BooleanCrystal.class, false, location(0, 9));
    }

    @Test
    @Order(8)
    public void testParser_Expression_LogicalNot_OnGroupingExpression_WithLogicalExpression() {
        String sourceString = "'[false \" true]";

        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        // 1. first expression (unary: "'[false \" true]")
        UnaryExpression unaryExpression = assertUnaryExpression(statements.get(0), location(0, 0));
        testOperator(unaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 0));

        // 2. unary operand (grouping: "[false \" true]")
        GroupingExpression groupingExpression = assertGroupingExpression(unaryExpression.getOperand(), location(0, 1));

        // 3. inner expression (logical: "false \" true")
        BooleanLogicExpression logicExpression = assertLogicalExpression(groupingExpression.getExpression(),location(0, 2));
        testOperator(logicExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 8));
        testLiteralExpression(logicExpression.getLeft(), BooleanCrystal.class, false, location(0, 2));
        testLiteralExpression(logicExpression.getRight(), BooleanCrystal.class, true, location(0, 10));
    }

    @Test
    @Order(9)
    public void testParser_Expression_LogicalNot_InGroupingExpression_WithLogicalExpression() {
        String sourceString = "['true ^ 'false]";

        List<Statement> statements = lexAndParse(sourceString);
        assertStatementCount(statements, 1);

        // 1. first expression (grouping: "['true ^ 'false']")
        GroupingExpression groupingExpression = assertGroupingExpression(statements.get(0), location(0, 0));

        // 2. inner expression (logical: "'true ^ 'false'")
        BooleanLogicExpression logicExpression = assertLogicalExpression(groupingExpression.getExpression(),location(0, 1));
        testOperator(logicExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 7));

        // 3. left operand (unary: "'true")
        UnaryExpression leftUnaryExpression = assertUnaryExpression(logicExpression.getLeft(), location(0, 1));
        testOperator(leftUnaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 1));
        testLiteralExpression(leftUnaryExpression.getOperand(), BooleanCrystal.class, true, location(0, 2));

        // 4. right operand (unary: "'false")
        UnaryExpression rightUnaryExpression = assertUnaryExpression(logicExpression.getRight(), location(0, 9));
        testOperator(rightUnaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 9));
        testLiteralExpression(rightUnaryExpression.getOperand(), BooleanCrystal.class, false, location(0, 10));
    }
}
