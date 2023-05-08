package com.atonement.crystals.dnr.vikari.interpreter.parser;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.literal.number.LongLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonement.crystals.dnr.vikari.core.expression.Expression;
import com.atonement.crystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonement.crystals.dnr.vikari.error.SyntaxError;
import com.atonement.crystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonement.crystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Helper methods for Parser tests.
 */
public class ParserTest_Utils {
    public static void testIntegerLiteralExpression(Expression expr, Object expectedValue) {
        assertEquals(LiteralExpression.class, expr.getClass(), "Unexpected expression type.");

        LiteralExpression literalExpression = (LiteralExpression) expr;
        AtonementCrystal value = literalExpression.getValue();
        assertEquals(LongLiteralCrystal.class, value.getClass(), "Unexpected literal type.");

        LongLiteralCrystal number = (LongLiteralCrystal) value;
        assertEquals(expectedValue, number.getValue(), "Unexpected literal value.");
    }

    public static void testBinaryExpression(Expression expr,
                                            Class<? extends BinaryOperatorCrystal> expectedOperatorClass,
                                            long expectedLeft, long expectedRight) {
        assertEquals(BinaryExpression.class, expr.getClass(), "Unexpected expression type.");
        BinaryExpression binaryExpression = (BinaryExpression) expr;

        Expression left = binaryExpression.getLeft();
        BinaryOperatorCrystal operator = binaryExpression.getOperator();
        Expression right = binaryExpression.getRight();

        assertEquals(expectedOperatorClass, operator.getClass(), "Unexpected operator type.");
        testIntegerLiteralExpression(left, expectedLeft);
        testIntegerLiteralExpression(right, expectedRight);
    }

    public static void testSyntaxError(SyntaxError syntaxError, CoordinatePair expectedLocation, String expectedLine,
                                       String partialErrorMessage) {
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains(partialErrorMessage), "Unexpected syntax error message.");
    }

    public static void assertNoSyntaxErrors(SyntaxErrorReporter syntaxErrorReporter) {
        if(syntaxErrorReporter.hasErrors()) {
            syntaxErrorReporter.reportErrors();
            fail("Expected no syntax errors for test case.");
        }
    }
}
