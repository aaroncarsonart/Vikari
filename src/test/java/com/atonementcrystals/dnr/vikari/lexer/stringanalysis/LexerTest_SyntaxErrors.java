package com.atonementcrystals.dnr.vikari.lexer.stringanalysis;

import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.File;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Ensure multiple error types can all occur together, and be reported accurately
 * all for the same source string.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_SyntaxErrors {

    @Test
    @Order(1)
    public void testSyntaxErrorCombos_multipleBacktickQuotations_andCaptureQuotation() {
        String sourceString = "a << `foo\n" +
                              "`z\tz` << a * 2\n" +
                              "bar:String << `  `\n" +
                              ":``baz``:``buzz:";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected syntax errors for multiple error types.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 4;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = new CoordinatePair(0, 5);
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = "a << `foo";
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("backtick"), "Unexpected syntax error message.");

        // Syntax Error 2
        syntaxError = syntaxErrors.get(1);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(1, 1);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = "`z\tz` << a * 2";
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("tab"), "Unexpected syntax error message.");

        // Syntax Error 3
        syntaxError = syntaxErrors.get(2);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(2, 15);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = "bar:String << `  `";
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("whitespace"), "Unexpected syntax error message.");

        // Syntax Error 4
        syntaxError = syntaxErrors.get(3);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(3, 9);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = ":``baz``:``buzz:";
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("capture quotation"), "Unexpected syntax error message.");
    }

    @Test
    @Order(2)
    public void testSyntaxErrorCombos_multipleErrorsOnSameLine_andCommentSuffix() {
        String sourceString = "`z\tz` << `foo\n" +
                              "~:Unclosed comment.";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected syntax errors for multiple error types.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 3;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = new CoordinatePair(0, 1);
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = "`z\tz` << `foo";
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("tab"), "Unexpected syntax error message.");

        // Syntax Error 2
        syntaxError = syntaxErrors.get(1);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(0, 9);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = "`z\tz` << `foo";
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("backtick"), "Unexpected syntax error message.");

        // Syntax Error 3
        syntaxError = syntaxErrors.get(2);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(1, 0);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = "~:Unclosed comment.";
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("comment suffix"), "Unexpected syntax error message.");
    }

    @Test
    @Order(3)
    public void testSyntaxErrors_tabIndentedCode() {
        String sourceString = "\t\t`z` << `foo\n" +
                              "\t\t~:Unclosed comment.";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        lexer.collapseTokens(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected syntax errors for multiple error types.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 2;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = new CoordinatePair(0, 9);
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = "\t\t`z` << `foo";
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("backtick"), "Unexpected syntax error message.");

        // Syntax Error 2
        syntaxError = syntaxErrors.get(1);
        expectedFile = null;
        actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        expectedLocation = new CoordinatePair(1, 2);
        actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        expectedLine = "\t\t~:Unclosed comment.";
        actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("comment suffix"), "Unexpected syntax error message.");
    }
}
