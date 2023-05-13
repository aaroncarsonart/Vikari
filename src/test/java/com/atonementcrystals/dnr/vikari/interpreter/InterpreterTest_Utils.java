package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class InterpreterTest_Utils {

    /**
     * Helper method to efficiently test a single Vikari expression statement returning a numeric value.
     * @param sourceString The Vikari source code to execute.
     * @param expectedValue The expected value for the result.
     * @param expectedClass The expected type of the result.
     */
    public static void testVikariExpression(String sourceString, Object expectedValue,
                                            Class<? extends NumberCrystal> expectedClass) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);
        assertEquals(1, parsedStatements.size(), "Unexpected number of statements");

        // statement 1
        Statement statement = parsedStatements.get(0);
        AtonementCrystal crystal = interpreter.execute(statement);
        TestUtils.testNumberCrystal(crystal, expectedValue, expectedClass);
    }

    /**
     * Run a series of Vikari code statements, and get the final result of the final statement.
     * @param sourceString The Vikari source code string to run.
     * @return The result of evaluating the final statement.
     */
    public static AtonementCrystal executeVikariCode(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        AtonementCrystal crystal = null;
        for (Statement statement : parsedStatements) {
            crystal = interpreter.execute(statement);
        }
        return crystal;
    }
}
