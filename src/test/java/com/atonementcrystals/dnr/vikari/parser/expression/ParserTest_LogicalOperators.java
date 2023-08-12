package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalAndOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalOrOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BooleanLogicExpression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Base;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testSyntaxError;
import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.*;

public class ParserTest_LogicalOperators extends ParserTest_Base {

    @Test
    @Order(1)
    public void testParser_Expression_LogicalOperators_And() {
        List<Statement> statements = lexAndParse("true ^ false");
        assertStatementCount(statements, 1);

        BooleanLogicExpression logicalExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        testLiteralExpression(logicalExpression.getLeft(), BooleanCrystal.class, true, location(0, 0));
        testOperator(logicalExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 5));
        testLiteralExpression(logicalExpression.getRight(), BooleanCrystal.class, false, location(0, 7));
    }

    @Test
    @Order(2)
    public void testParser_Expression_LogicalOperators_Or() {
        List<Statement> statements = lexAndParse("false \" true");
        assertStatementCount(statements, 1);

        BooleanLogicExpression logicalExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        testLiteralExpression(logicalExpression.getLeft(), BooleanCrystal.class, false, location(0, 0));
        testOperator(logicalExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 6));
        testLiteralExpression(logicalExpression.getRight(), BooleanCrystal.class, true, location(0, 8));
    }

    @Test
    @Order(3)
    public void testParser_Expression_LogicalOperators_TrueAndFalseOrTrue() {
        List<Statement> statements = lexAndParse("true ^ false \" true");
        assertStatementCount(statements, 1);

        // Outer OR expression
        BooleanLogicExpression orExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        testOperator(orExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 13));
        testLiteralExpression(orExpression.getRight(), BooleanCrystal.class, true, location(0, 15));

        // Inner AND expression
        BooleanLogicExpression andExpression = assertLogicalExpression(orExpression.getLeft(), location(0, 0));

        testLiteralExpression(andExpression.getLeft(), BooleanCrystal.class, true, location(0, 0));
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 5));
        testLiteralExpression(andExpression.getRight(), BooleanCrystal.class, false, location(0, 7));
    }

    @Test
    @Order(4)
    public void testParser_Expression_LogicalOperators_FalseOrTrueAndFalse() {
        List<Statement> statements = lexAndParse("false \" true ^ false");
        assertStatementCount(statements, 1);

        // Outer OR expression
        BooleanLogicExpression orExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        testLiteralExpression(orExpression.getLeft(), BooleanCrystal.class, false, location(0, 0));
        testOperator(orExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 6));

        // Inner AND expression
        BooleanLogicExpression andExpression = assertLogicalExpression(orExpression.getRight(), location(0, 8));

        testLiteralExpression(andExpression.getLeft(), BooleanCrystal.class, true, location(0, 8));
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 13));
        testLiteralExpression(andExpression.getRight(), BooleanCrystal.class, false, location(0, 15));
    }

    @Test
    @Order(5)
    public void testParser_Expression_LogicalOperators_TrueAndTrueAndTrue() {
        List<Statement> statements = lexAndParse("true ^ true ^ true");
        assertStatementCount(statements, 1);

        // Outer AND expression
        BooleanLogicExpression andExpression1 = assertLogicalExpression(statements.get(0), location(0, 0));

        testOperator(andExpression1.getOperator(), LogicalAndOperatorCrystal.class, location(0, 12));
        testLiteralExpression(andExpression1.getRight(), BooleanCrystal.class, true, location(0, 14));

        // Inner AND expression
        BooleanLogicExpression andExpression2 = assertLogicalExpression(andExpression1.getLeft(), location(0, 0));

        testLiteralExpression(andExpression2.getLeft(), BooleanCrystal.class, true, location(0, 0));
        testOperator(andExpression2.getOperator(), LogicalAndOperatorCrystal.class, location(0, 5));
        testLiteralExpression(andExpression2.getRight(), BooleanCrystal.class, true, location(0, 7));
    }

    @Test
    @Order(6)
    public void testParser_Expression_LogicalOperators_FalseOrFalseOrFalse() {
        List<Statement> statements = lexAndParse("false \" false \" false");
        assertStatementCount(statements, 1);

        // Outer OR expression
        BooleanLogicExpression orExpression1 = assertLogicalExpression(statements.get(0), location(0, 0));

        testOperator(orExpression1.getOperator(), LogicalOrOperatorCrystal.class, location(0, 14));
        testLiteralExpression(orExpression1.getRight(), BooleanCrystal.class, false, location(0, 16));

        // Inner OR expression
        BooleanLogicExpression orExpression2 = assertLogicalExpression(orExpression1.getLeft(), location(0, 0));

        testLiteralExpression(orExpression2.getLeft(), BooleanCrystal.class, false, location(0, 0));
        testOperator(orExpression2.getOperator(), LogicalOrOperatorCrystal.class, location(0, 6));
        testLiteralExpression(orExpression2.getRight(), BooleanCrystal.class, false, location(0, 8));
    }

    @Test
    @Order(7)
    public void testParser_Expression_LogicalOperators_OneGroupedOperand() {
        // -----------------
        // grouping OR value
        // -----------------
        List<Statement> statements = lexAndParse("[true ^ false] \" true");
        assertStatementCount(statements, 1);

        BooleanLogicExpression orExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        // left operand
        GroupingExpression groupingExpression = assertGroupingExpression(orExpression.getLeft(), location(0, 0));
        BooleanLogicExpression andExpression = assertLogicalExpression(groupingExpression.getExpression(), location(0, 1));

        testLiteralExpression(andExpression.getLeft(), BooleanCrystal.class, true, location(0, 1));
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 6));
        testLiteralExpression(andExpression.getRight(), BooleanCrystal.class, false, location(0, 8));

        // right operand
        testOperator(orExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 15));
        testLiteralExpression(orExpression.getRight(), BooleanCrystal.class, true, location(0, 17));

        // -----------------
        // value OR grouping
        // -----------------
        statements = lexAndParse("true \" [false ^ true]");
        assertStatementCount(statements, 1);

        orExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        // left operand
        testLiteralExpression(orExpression.getLeft(), BooleanCrystal.class, true, location(0, 0));
        testOperator(orExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 5));

        // right operand
        groupingExpression = assertGroupingExpression(orExpression.getRight(), location(0, 7));
        andExpression = assertLogicalExpression(groupingExpression.getExpression(), location(0, 8));

        testLiteralExpression(andExpression.getLeft(), BooleanCrystal.class, false, location(0, 8));
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 14));
        testLiteralExpression(andExpression.getRight(), BooleanCrystal.class, true, location(0, 16));

        // ------------------
        // grouping AND value
        // ------------------
        statements = lexAndParse("[true \" false] ^ true");
        assertStatementCount(statements, 1);

        andExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        // left operand
        groupingExpression = assertGroupingExpression(andExpression.getLeft(), location(0, 0));
        orExpression = assertLogicalExpression(groupingExpression.getExpression(), location(0, 1));

        testLiteralExpression(orExpression.getLeft(), BooleanCrystal.class, true, location(0, 1));
        testOperator(orExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 6));
        testLiteralExpression(orExpression.getRight(), BooleanCrystal.class, false, location(0, 8));

        // right operand
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 15));
        testLiteralExpression(andExpression.getRight(), BooleanCrystal.class, true, location(0, 17));

        // ------------------
        // value AND grouping
        // ------------------
        statements = lexAndParse("true ^ [false \" true]");
        assertStatementCount(statements, 1);

        andExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        // left operand
        testLiteralExpression(andExpression.getLeft(), BooleanCrystal.class, true, location(0, 0));
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 5));

        // right operand
        groupingExpression = assertGroupingExpression(andExpression.getRight(), location(0, 7));
        orExpression = assertLogicalExpression(groupingExpression.getExpression(), location(0, 8));

        testLiteralExpression(orExpression.getLeft(), BooleanCrystal.class, false, location(0, 8));
        testOperator(orExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 14));
        testLiteralExpression(orExpression.getRight(), BooleanCrystal.class, true, location(0, 16));
    }

    @Test
    @Order(8)
    public void testParser_Expression_LogicalOperators_TwoGroupedOperands() {
        // --------------------
        // grouping OR grouping
        // --------------------
        List<Statement> statements = lexAndParse("[true ^ false] \" [false ^ true]");
        assertStatementCount(statements, 1);

        BooleanLogicExpression orExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        // left operand
        GroupingExpression groupingExpression1 = assertGroupingExpression(orExpression.getLeft(), location(0, 0));
        BooleanLogicExpression andExpression1 = assertLogicalExpression(groupingExpression1.getExpression(), location(0, 1));

        testLiteralExpression(andExpression1.getLeft(), BooleanCrystal.class, true, location(0, 1));
        testOperator(andExpression1.getOperator(), LogicalAndOperatorCrystal.class, location(0, 6));
        testLiteralExpression(andExpression1.getRight(), BooleanCrystal.class, false, location(0, 8));

        // operator
        testOperator(orExpression.getOperator(), LogicalOrOperatorCrystal.class, location(0, 15));

        // right operand
        GroupingExpression groupingExpression2 = assertGroupingExpression(orExpression.getRight(), location(0, 17));
        BooleanLogicExpression andExpression2 = assertLogicalExpression(groupingExpression2.getExpression(), location(0, 18));

        testLiteralExpression(andExpression2.getLeft(), BooleanCrystal.class, false, location(0, 18));
        testOperator(andExpression2.getOperator(), LogicalAndOperatorCrystal.class, location(0, 24));
        testLiteralExpression(andExpression2.getRight(), BooleanCrystal.class, true, location(0, 26));

        // ---------------------
        // grouping AND grouping
        // ---------------------
        statements = lexAndParse("[true \" false] ^ [false \" true]");
        assertStatementCount(statements, 1);

        BooleanLogicExpression andExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        // left operand
        groupingExpression1 = assertGroupingExpression(andExpression.getLeft(), location(0, 0));
        BooleanLogicExpression orExpression1 = assertLogicalExpression(groupingExpression1.getExpression(), location(0, 1));

        testLiteralExpression(orExpression1.getLeft(), BooleanCrystal.class, true, location(0, 1));
        testOperator(orExpression1.getOperator(), LogicalOrOperatorCrystal.class, location(0, 6));
        testLiteralExpression(orExpression1.getRight(), BooleanCrystal.class, false, location(0, 8));

        // operator
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 15));

        // right operand
        groupingExpression2 = assertGroupingExpression(andExpression.getRight(), location(0, 17));
        BooleanLogicExpression orExpression2 = assertLogicalExpression(groupingExpression2.getExpression(), location(0, 18));

        testLiteralExpression(orExpression2.getLeft(), BooleanCrystal.class, false, location(0, 18));
        testOperator(orExpression2.getOperator(), LogicalOrOperatorCrystal.class, location(0, 24));
        testLiteralExpression(orExpression2.getRight(), BooleanCrystal.class, true, location(0, 26));
    }

    @Test
    @Order(9)
    public void testParser_Expression_LogicalOperators_UnknownVariableAsOperand() {
        String sourceString = "false ^ bar";
        List<Statement> statements = lexAndParse(sourceString, 1);
        assertStatementCount(statements, 1);

        // statements
        BooleanLogicExpression andExpression = assertLogicalExpression(statements.get(0), location(0, 0));

        testLiteralExpression(andExpression.getLeft(), BooleanCrystal.class, false, location(0, 0));
        testOperator(andExpression.getOperator(), LogicalAndOperatorCrystal.class, location(0, 6));
        testVariableExpression(andExpression.getRight(), "bar", VikariType.INVALID, VikariType.INVALID, location(0, 8));

        // errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 8), sourceString, "Undefined variable reference.");
    }
}
