package com.atonement.crystals.dnr.vikari.interpreter.parser.expression;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.literal.number.LongLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
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

import static com.atonement.crystals.dnr.vikari.interpreter.parser.ParserTest_Utils.assertNoSyntaxErrors;
import static com.atonement.crystals.dnr.vikari.interpreter.parser.ParserTest_Utils.testSyntaxError;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Grouping {

    @Test
    @Order(1)
    public void testGrouping_IntegerLiteral() {
        String sourceString = "[5]";

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

        // 1. first expression (grouping)
        Expression expression = expressionStatement.getExpression();
        assertEquals(GroupingExpression.class, expression.getClass(), "Unexpected expression type.");
        GroupingExpression groupingExpression = (GroupingExpression) expression;

        // 2. inner expression (literal)
        Expression innerExpression = groupingExpression.getExpression();
        assertEquals(LiteralExpression.class, innerExpression.getClass(), "Unexpected expression type.");
        LiteralExpression literalExpression = (LiteralExpression) innerExpression;

        AtonementCrystal value = literalExpression.getValue();
        assertEquals(LongLiteralCrystal.class, value.getClass(), "Unexpected literal type.");

        LongLiteralCrystal number = (LongLiteralCrystal) value;
        assertEquals(5, number.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(2)
    public void testGrouping_BinaryExpression() {
        String sourceString = "[22 / 7]";

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

        // 1. first expression (grouping: "[22 / 7]")
        Expression expression = expressionStatement.getExpression();
        assertEquals(GroupingExpression.class, expression.getClass(), "Unexpected expression type.");
        GroupingExpression groupingExpression = (GroupingExpression) expression;

        // 2. inner expression (binary: "22 / 7")
        Expression innerExpression = groupingExpression.getExpression();
        assertEquals(BinaryExpression.class, innerExpression.getClass(), "Unexpected expression type.");
        BinaryExpression binaryExpression = (BinaryExpression) innerExpression;

        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, left.getClass(), "Unexpected expression type.");
        assertEquals(LeftDivideOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, right.getClass(), "Unexpected expression type.");

        // 3. left operand (literal: "22")
        AtonementCrystal leftOperand = ((LiteralExpression) left).getValue();
        assertEquals(LongLiteralCrystal.class, leftOperand.getClass(), "Unexpected literal type.");

        LongLiteralCrystal leftNumber = (LongLiteralCrystal) leftOperand;
        assertEquals(22, leftNumber.getValue(), "Unexpected literal value.");

        // 4. right operand (literal: 7)
        AtonementCrystal rightOperand = ((LiteralExpression) right).getValue();
        assertEquals(LongLiteralCrystal.class, rightOperand.getClass(), "Unexpected literal type.");

        LongLiteralCrystal rightNumber = (LongLiteralCrystal) rightOperand;
        assertEquals(7, rightNumber.getValue(), "Unexpected literal value.");
    }

    @Test
    @Order(3)
    public void testGrouping_MissingOpeningSquareBracket() {
        String sourceString = "22 / 7]";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        assertTrue(syntaxErrorReporter.hasErrors(), "Expected a syntax error for missing opening square bracket.");
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        assertEquals(1, syntaxErrors.size(), "Unexpected number of syntax errors.");

        // Syntax Error 1
        testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 6), sourceString, "Expected expression.");
    }

    @Test
    @Order(4)
    public void testGrouping_MissingClosingSquareBracket() {
        String sourceString = "[22 / 7";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        assertTrue(syntaxErrorReporter.hasErrors(), "Expected a syntax error for missing opening square bracket.");
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        assertEquals(1, syntaxErrors.size(), "Unexpected number of syntax errors.");

        // Syntax Error 1
        testSyntaxError(syntaxErrors.get(0), new CoordinatePair(0, 6), sourceString, "Expected `]` after expression.");
    }
}
