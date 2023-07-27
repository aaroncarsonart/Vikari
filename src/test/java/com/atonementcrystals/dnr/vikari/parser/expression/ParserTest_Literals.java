package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Literals {

    @Test
    @Order(1)
    public void testParser_Expression_Literals_Integer() {
        String sourceString = "2";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
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
        testIntegerLiteralExpression(expression, Integer.valueOf(2));
    }

    @Test
    @Order(2)
    public void testParser_Expression_Literals_Long() {
        String sourceString = "22L";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
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
        testLongLiteralExpression(expression, Long.valueOf(22L));
    }

    @Test
    @Order(3)
    public void testParser_Expression_Literals_BigInteger() {
        String sourceString = "512B";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
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
        testBigIntegerLiteralExpression(expression, new BigInteger("512"));
    }

    @Test
    @Order(4)
    public void testParser_Expression_Literals_Float() {
        String sourceString = "3.14F";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
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
        testFloatLiteralExpression(expression, Float.valueOf(3.14F));
    }

    @Test
    @Order(5)
    public void testParser_Expression_Literals_Double() {
        String sourceString = "6.28D";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
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
        testDoubleLiteralExpression(expression, Double.valueOf(6.28D));
    }

    @Test
    @Order(6)
    public void testParser_Expression_Literals_BigDecimal() {
        String sourceString = "9.42B";

        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
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
        testBigDecimalLiteralExpression(expression, new BigDecimal("9.42", Arithmetic.getMathContext()));
    }

    @Test
    @Order(7)
    public void testParser_Expression_Literals_NullKeyword() {
        List<Statement> parsedStatements = lexAndParse("null");

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        testNullKeyword(expression, location(0, 0));
    }

    @Test
    @Order(8)
    public void testParser_Expression_Literals_NullLiteral_Sword() {
        List<Statement> parsedStatements = lexAndParse("_");

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        testNullSwordLiteral(expression, location(0, 0), 1);
    }

    private void testNullLiteralExpression(String sourceString,
                                           Class<? extends NumberCrystal<? extends Number>> valueType,
                                           Number numericValue) {

        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        // statement 1
        Statement statement = parsedStatements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;

        // first expression
        Expression expression = expressionStatement.getExpression();
        boolean isNegative = numericValue.toString().charAt(0) == '-';
        int column = isNegative ? 4 : 3;
        testNullLiteralExpression_SingleNumberOperand(expression, location(0, 0), location(0, column), valueType, numericValue);
    }

    @Test
    @Order(9)
    public void testParser_Expression_Literals_NullLiteralExpression() {
        testNullLiteralExpression("__[-1]__", IntegerCrystal.class, -1);
    }

    @Test
    @Order(10)
    public void testParser_Expression_Literals_NullLiteralExpression_AllNumericTypes() {
        testNullLiteralExpression("__[5]__", IntegerCrystal.class, 5);
        testNullLiteralExpression("__[5L]__", LongCrystal.class, 5L);
        testNullLiteralExpression("__[5B]__", BigIntegerCrystal.class, new BigInteger("5"));
        testNullLiteralExpression("__[5.0F]__", FloatCrystal.class, 5F);
        testNullLiteralExpression("__[5.0D]__", DoubleCrystal.class, 5D);
        testNullLiteralExpression("__[5.0B]__", BigDecimalCrystal.class, new BigDecimal("5.0", Arithmetic.getMathContext()));
    }
}
