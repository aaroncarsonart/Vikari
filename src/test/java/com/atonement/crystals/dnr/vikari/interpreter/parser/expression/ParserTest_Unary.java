package com.atonement.crystals.dnr.vikari.interpreter.parser.expression;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.expression.Expression;
import com.atonement.crystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonement.crystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonement.crystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonement.crystals.dnr.vikari.core.statement.Statement;
import com.atonement.crystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonement.crystals.dnr.vikari.interpreter.Lexer;
import com.atonement.crystals.dnr.vikari.interpreter.Parser;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonement.crystals.dnr.vikari.interpreter.parser.ParserTest_Utils.assertNoSyntaxErrors;
import static com.atonement.crystals.dnr.vikari.interpreter.parser.ParserTest_Utils.testBinaryExpression;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Note that basic negation of numeric literals (i.e.: "-3") is handled by the lexer.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Unary {

    @Test
    @Order(1)
    public void testNegateGrouping() {
        String sourceString = "-[5 - 3]";

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
        testBinaryExpression(innerExpression, SubtractOperatorCrystal.class, 5L, 3L);
    }
}
