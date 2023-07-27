package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Base;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Variables extends ParserTest_Base {

    /**
     * This is a strange error case. Bare expression statements like this will never be used in production code except
     * for single-statement return values of functions. But yet it is unavoidable that "foo + 5" will return a different
     * error message than "5 + foo" without significant refactoring. Because of how the Parser detects a variable
     * declaration statement. (Any statement beginning with an undefined reference variable.)
     */
    @Test
    @Order(1)
    public void testParser_Expression_UndefinedVariableReference_LeadingWithVariable() {
        String sourceString = "foo + 5";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), sourceString, "Unexpected token(s) in " +
                "variable declaration statement");

        int expectedStatementCount = 1;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // This is a malformed variable declaration. Simply assert the type.
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
    }

    @Test
    @Order(2)
    public void testParser_Expression_UndefinedVariableReference_VariableAtEnd() {
        String sourceString = "5 + foo";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), sourceString, "Undefined variable reference.");

        int expectedStatementCount = 1;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
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
        assertEquals(VariableExpression.class, right.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((LiteralExpression) left).getValue();
        AtonementCrystal rightOperand = ((VariableExpression) right).getReference();

        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(ReferenceCrystal.class, rightOperand.getClass(), "Unexpected variable type.");

        IntegerCrystal number = (IntegerCrystal) leftOperand;
        ReferenceCrystal variable = (ReferenceCrystal) rightOperand;

        assertEquals(5, number.getValue(), "Unexpected literal value.");
        assertEquals("foo", variable.getIdentifier(), "Unexpected reference identifier.");
    }

    @Test
    @Order(3)
    public void testParser_Expression_VariableExpressionThenPrimary() {
        String sourceString = "foo,foo";

        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testVariableExpression(statements.get(1), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 4));
    }

    @Test
    @Order(4)
    public void testParser_Expression_Variable_ArithmeticExpressionTypeError() {
        String sourceString = """
                foo
                foo + 5
                5 + foo
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(1, 0), "foo + 5", "Arithmetic " +
                "expression expects a Number for operands.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(2, 4), "5 + foo", "Arithmetic " +
                "expression expects a Number for operands.");

        int expectedStatementCount = 3;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration
        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));

        // arithmetic expressions

        // statement 2
        Statement statement = statements.get(1);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression: "foo + 5"
        Expression expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) expression;
        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(VariableExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(AddOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((VariableExpression) left).getReference();
        AtonementCrystal rightOperand = ((LiteralExpression) right).getValue();

        assertEquals(ReferenceCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(IntegerCrystal.class, rightOperand.getClass(), "Unexpected variable type.");

        ReferenceCrystal variable = (ReferenceCrystal) leftOperand;
        IntegerCrystal number = (IntegerCrystal) rightOperand;

        assertEquals("foo", variable.getIdentifier(), "Unexpected reference identifier.");
        assertEquals(5, number.getValue(), "Unexpected literal value.");

        // statement 3
        statement = statements.get(2);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        expressionStatement = (ExpressionStatement) statement;

        // first expression
        expression = expressionStatement.getExpression();
        assertEquals(BinaryExpression.class, expression.getClass(), "Unexpected expression type.");

        binaryExpression = (BinaryExpression) expression;
        left = binaryExpression.getLeft();
        operator = binaryExpression.getOperator();
        right = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(AddOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(VariableExpression.class, right.getClass(), "Unexpected expression type.");

        leftOperand = ((LiteralExpression) left).getValue();
        rightOperand = ((VariableExpression) right).getReference();

        assertEquals(IntegerCrystal.class, leftOperand.getClass(), "Unexpected literal type.");
        assertEquals(ReferenceCrystal.class, rightOperand.getClass(), "Unexpected variable type.");

        number = (IntegerCrystal) leftOperand;
        variable = (ReferenceCrystal) rightOperand;

        assertEquals(5, number.getValue(), "Unexpected literal value.");
        assertEquals("foo", variable.getIdentifier(), "Unexpected reference identifier.");
    }
}
