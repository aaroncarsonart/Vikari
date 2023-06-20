package com.atonementcrystals.dnr.vikari.lexer;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class LexerTestUtils {

    // -----------------------------
    // 1: String token test methods.
    // -----------------------------

    public static List<List<String>> lexAsTokens(String sourceString, int expectedStatementCount,
                                                 int... statementSizes) {
        return lexAsTokens(sourceString, expectedStatementCount, new SyntaxErrorReporter(), 0, statementSizes);
    }

    public static List<List<String>> lexAsTokens(String sourceString, int expectedStatementCount,
                                                 SyntaxErrorReporter syntaxErrorReporter, int expectedErrorCount,
                                                 int... statementSizes) {
        Lexer lexer = new Lexer();

        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> statements = lexer.lexToStringTokens(sourceString);

        if (expectedErrorCount == 0 && syntaxErrorReporter.hasErrors()) {
            syntaxErrorReporter.reportErrors();
            fail("Expected no syntax errors.");
        } else {
            int actualErrorCount = syntaxErrorReporter.getSyntaxErrors().size();
            assertEquals(expectedErrorCount, actualErrorCount, "Unexpected number of syntax errors.");
        }

        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        for (int i = 0; i < statementSizes.length; i++) {
            List<String> statement = statements.get(i);

            int expectedStatementSize = statementSizes[i];
            int actualStatementSize = statement.size();
            assertEquals(expectedStatementSize, actualStatementSize, "Unexpected number of tokens.");
        }

        return statements;
    }

    public static List<String> lexSingleStatementAsTokens(String sourceString, int expectedSize) {
        List<List<String>> statements = lexAsTokens(sourceString, 1, expectedSize);
        return statements.get(0);
    }

    public static List<String> lexSingleStatementAsTokens(String sourceString, int expectedSize,
                                                          SyntaxErrorReporter syntaxErrorReporter,
                                                          int expectedErrorCount) {
        List<List<String>> statements = lexAsTokens(sourceString, 1, syntaxErrorReporter, expectedErrorCount, expectedSize);
        return statements.get(0);
    }

    public static void testToken(String token, String expectedIdentifier) {
        assertEquals(expectedIdentifier, token, "Unexpected String token value.");
    }

    /**
     * This is a self-documenting method which simply returns its arguments as an array. This makes the varargs
     * argument statementSizes for lexAsTokens() more readable and clear as to its purpose when it is called in
     * test cases than merely a list of numbers after the expectedStatementCount argument.
     *
     * @param tokenCounts The list of expected token counts for each sequential statement.
     * @return The array of values passed as arguments to this method.
     */
    public static int[] tokenCounts(int... tokenCounts) {
        return tokenCounts;
    }

    // ---------------------------------
    // 2: AtonementCrystal test methods.
    // ---------------------------------

    public static List<List<AtonementCrystal>> lex(String sourceString, int expectedStatementCount,
                                                   int... statementSizes) {
        return lex(sourceString, expectedStatementCount, new SyntaxErrorReporter(), 0, statementSizes);
    }
    public static List<List<AtonementCrystal>> lex(String sourceString, int expectedStatementCount,
                                                   SyntaxErrorReporter syntaxErrorReporter, int expectedErrorCount,
                                                   int... statementSizes) {
        Lexer lexer = new Lexer();

        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        if (expectedErrorCount == 0 && syntaxErrorReporter.hasErrors()) {
            syntaxErrorReporter.reportErrors();
            fail("Expected no syntax errors.");
        } else {
            int actualErrorCount = syntaxErrorReporter.getSyntaxErrors().size();
            assertEquals(expectedErrorCount, actualErrorCount, "Unexpected number of syntax errors.");
        }

        int actualStatementCount = statementsOfCrystals.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        for (int i = 0; i < statementSizes.length; i++) {
            List<AtonementCrystal> statement = statementsOfCrystals.get(i);

            int expectedStatementSize = statementSizes[i];
            int actualStatementSize = statement.size();
            assertEquals(expectedStatementSize, actualStatementSize, "Unexpected number of tokens.");
        }

        return statementsOfCrystals;
    }

    public static List<AtonementCrystal> lexSingleStatement(String sourceString, int expectedSize) {
        List<List<AtonementCrystal>> statementsOfCrystals = lex(sourceString, 1, expectedSize);
        return statementsOfCrystals.get(0);
    }

    public static List<AtonementCrystal> lexSingleStatement(String sourceString, int expectedSize,
                                                            SyntaxErrorReporter syntaxErrorReporter,
                                                            int expectedErrorCount) {
        List<List<AtonementCrystal>> statementsOfCrystals = lex(sourceString, 1, syntaxErrorReporter,
                expectedErrorCount, crystalCounts(expectedSize));
        return statementsOfCrystals.get(0);
    }

    /**
     * This is a self-documenting method which simply returns its arguments as an array. This makes the varargs
     * argument statementSizes for lexAsTokens() more readable and clear as to its purpose when it is called in
     * test cases than merely a list of numbers after the expectedStatementCount argument.
     *
     * @param crystalCounts The list of expected token counts for each sequential statement.
     * @return The array of values passed as arguments to this method.
     */
    public static int[] crystalCounts(int... crystalCounts) {
        return crystalCounts;
    }
}
