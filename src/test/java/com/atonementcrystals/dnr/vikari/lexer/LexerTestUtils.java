package com.atonementcrystals.dnr.vikari.lexer;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;

import java.util.List;
import java.util.stream.Collectors;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static org.junit.jupiter.api.Assertions.*;

public class LexerTestUtils {

    private static boolean enableWarnings = false;

    public static void setEnableWarnings(boolean enableWarnings) {
        LexerTestUtils.enableWarnings = enableWarnings;
    }

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

        if (expectedErrorCount == 0) {
            assertNoSyntaxErrors(syntaxErrorReporter);
        } else {
            assertSyntaxErrors(syntaxErrorReporter, expectedErrorCount);
        }

        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        for (int i = 0; i < statementSizes.length; i++) {
            List<String> statement = statements.get(i);

            int expectedStatementSize = statementSizes[i];
            int actualStatementSize = statement.size();

            if(expectedStatementSize != actualStatementSize) {
                String tokensString = statement.stream()
                        .map(token -> "\"" + token + "\"")
                        .collect(Collectors.joining(", "));

                fail("Unexpected number of tokens in statement.\n" +
                        "Expected:  " + expectedStatementSize + "\n" +
                        "Actual:    " + actualStatementSize + "\n" +
                        "Statement: " + tokensString);
            }
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

    public enum CommentTokenType {
        SINGLE_LINE, START, MIDDLE, END;
    }

    public static void testComment(String commentToken, int expectedLength, CommentTokenType commentTokenType) {
        int actualLength = commentToken.length();
        assertEquals(expectedLength, actualLength, "Unexpected length of comment token.");

        String openingToken = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        String closingToken = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();

        int min;
        int max;

        switch (commentTokenType) {
            case SINGLE_LINE:
                assertTrue(commentToken.startsWith(openingToken));
                assertTrue(commentToken.endsWith(closingToken));
                assertTrue(commentToken.length() >= 4, "A single-line comment must be at least length 4.");

                min = openingToken.length();
                max = commentToken.length() - closingToken.length();
                break;
            case START:
                assertTrue(commentToken.startsWith(openingToken));
                min = openingToken.length();
                max = commentToken.length();
                break;
            case MIDDLE:
                min = 0;
                max = commentToken.length();
                break;
            case END:
                assertTrue(commentToken.endsWith(closingToken));
                min = 0;
                max = commentToken.length() - closingToken.length();
                break;
            default:
                throw new IllegalStateException("Unhandled CommentTokenType enum value: " + commentTokenType.name());
        }

        char expectedCharacter = '-';
        for (int i = min; i < max; i++) {
            char actualCharacter = commentToken.charAt(i);
            assertEquals(expectedCharacter, actualCharacter, "Expected comment token to be lexed as all dashes " +
                    "between opening and closing tokens.");
        }
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
        lexer.setCompilationWarningsEnabled(enableWarnings);

        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        if (expectedErrorCount == 0) {
            assertNoSyntaxErrors(syntaxErrorReporter);
        } else {
            assertSyntaxErrors(syntaxErrorReporter, expectedErrorCount);
        }

        int actualStatementCount = statementsOfCrystals.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected number of statements.");

        for (int i = 0; i < statementSizes.length; i++) {
            List<AtonementCrystal> statement = statementsOfCrystals.get(i);

            int expectedStatementSize = statementSizes[i];
            int actualStatementSize = statement.size();

            if(expectedStatementSize != actualStatementSize) {
                String crystalIdentifiersString = statement.stream()
                        .map(AtonementCrystal::getIdentifier)
                        .map(identifier -> "\"" + identifier + "\"")
                        .collect(Collectors.joining(", "));

                fail("Unexpected number of crystals in statement.\n" +
                        "Expected:  " + expectedStatementSize + "\n" +
                        "Actual:    " + actualStatementSize + "\n" +
                        "Statement: " + crystalIdentifiersString);
            }
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
