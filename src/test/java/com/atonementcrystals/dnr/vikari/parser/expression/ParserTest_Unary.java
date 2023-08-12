package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalNotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.NegateCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Note that basic negation of numeric literals (i.e.: "-3") is handled by the lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Unary {

    @Test
    @Order(1)
    public void testParser_Expression_NegateGrouping() {
        String sourceString = "-[5 - 3]";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // 1. first expression (unary: "-[5 - 3]")
        Expression expression = expressionStatement.getExpression();
        assertEquals(UnaryExpression.class, expression.getClass(), "Unexpected expression type.");
        UnaryExpression unaryExpression = (UnaryExpression) expression;

        // 2. right operand (grouping: "[5 - 3]")
        Expression rightOperand = unaryExpression.getOperand();
        assertEquals(GroupingExpression.class, rightOperand.getClass(), "Unexpected expression type.");
        GroupingExpression groupingExpression = (GroupingExpression) rightOperand;

        // 3. inner expression (subtract: "5 - 3")
        Expression innerExpression = groupingExpression.getExpression();
        ParserTest_Utils.testBinaryExpression(innerExpression, SubtractOperatorCrystal.class, 5, 3);
    }

    @Test
    @Order(2)
    public void testParser_Expression_Negate_InvalidOperand() {
        String sourceString = "-true";

        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        List<Statement> statements = ParserTest_Utils.lexAndParse(sourceString, errorReporter, 1);
        assertStatementCount(statements, 1);

        ExpressionStatement expressionStatement = assertExpressionStatement(statements.get(0), location(0, 0));
        UnaryExpression unaryExpression = assertUnaryExpression(expressionStatement.getExpression(), location(0, 0));
        testOperator(unaryExpression.getOperator(), NegateCrystal.class, location(0, 0));
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

        ExpressionStatement expressionStatement = assertExpressionStatement(statements.get(0), location(0, 0));
        UnaryExpression unaryExpression = assertUnaryExpression(expressionStatement.getExpression(), location(0, 0));
        testOperator(unaryExpression.getOperator(), LogicalNotOperatorCrystal.class, location(0, 0));
        testLiteralExpression(unaryExpression.getOperand(), IntegerCrystal.class, 3, location(0, 1));

        List<VikariError> syntaxErrors = errorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Logical not expression expects a " +
                "Boolean as its operand.");
    }
}
