package com.atonementcrystals.dnr.vikari.lexer.tokens;

import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testSyntaxError;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

public class LexerTest_StringTokens_InvalidCharacters {

    @Test
    @Order(1)
    public void testLexer_StringTokens_InvalidCharacters_CheckAllValidCharacters() {
        String sourceString = "~!@#$%^&*()_+1234567890-=\tQWERTYUIOPASDFGHJKLZXCVBNM qwertyuiopasdfghjklzxcvbnm{}|[]" +
                "\\:\";'<>?,./'`";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexSingleStatementAsTokens(sourceString, 37, syntaxErrorReporter, 1);
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();

        testSyntaxError(syntaxErrors.get(0), location(0, 96), sourceString, "Missing closing backtick quotation `.");
    }

    @Test
    @Order(2)
    public void testLexer_StringTokens_InvalidCharacters_SingleInvalidCharacters() {
        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length(); i++) {
            String sourceString = String.valueOf(invalidCharactersToTest.charAt(i));

            SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
            List<String> tokens = lexSingleStatementAsTokens(sourceString, 1, syntaxErrorReporter, 1);

            testToken(tokens.get(0), sourceString);

            List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
            testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Invalid characters.");
        }
    }

    @Test
    @Order(3)
    public void testLexer_StringTokens_InvalidCharacters_SingleInvalidCharacter_BeforeSingleValidCharacter() {
        String validCharacters = "~!@#$%^&*()_+1234567890-=QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm{}|[]" +
                "\\:\";'<>?,./`";

        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length(); i++) {
            String invalidCharacter = String.valueOf(invalidCharactersToTest.charAt(i));

            for (int j = 0; j < validCharacters.length(); j++) {
                String validCharacter = String.valueOf(validCharacters.charAt(j));
                boolean atBacktickCharacter = j == validCharacters.length() - 1;

                String sourceString = invalidCharacter + validCharacter;

                SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
                int expectedErrorCount = atBacktickCharacter ? 2 : 1;

                List<String> tokens = lexSingleStatementAsTokens(sourceString, 2, syntaxErrorReporter, expectedErrorCount);

                testToken(tokens.get(0), invalidCharacter);
                testToken(tokens.get(1), validCharacter);

                List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
                testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Invalid characters.");

                if (atBacktickCharacter) {
                    testSyntaxError(syntaxErrors.get(1), location(0, 1), sourceString, "Missing closing backtick quotation `.");
                }
            }
        }
    }

    @Test
    @Order(4)
    public void testLexer_StringTokens_InvalidCharacters_SingleInvalidCharacter_AfterSingleValidCharacter() {
        String validCharacters = "~!@#$%^&*()_+1234567890-=QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm{}|[]" +
                "\\:\";'<>?,./`";

        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length(); i++) {
            String invalidCharacter = String.valueOf(invalidCharactersToTest.charAt(i));

            for (int j = 0; j < validCharacters.length(); j++) {
                String validCharacter = String.valueOf(validCharacters.charAt(j));
                boolean atBacktickCharacter = j == validCharacters.length() - 1;

                String sourceString = validCharacter + invalidCharacter;
                SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
                int expectedTokenCount = atBacktickCharacter ? 1 : 2;

                List<String> tokens = lexSingleStatementAsTokens(sourceString, expectedTokenCount, syntaxErrorReporter, 1);

                if (atBacktickCharacter) {
                    testToken(tokens.get(0), sourceString);
                } else {
                    testToken(tokens.get(0), validCharacter);
                    testToken(tokens.get(1), invalidCharacter);
                }

                List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();

                if (atBacktickCharacter) {
                    testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Missing closing backtick quotation `.");
                } else {
                    testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Invalid characters.");
                }
            }
        }
    }

    @Test
    @Order(5)
    public void testLexer_StringTokens_InvalidCharacters_SingleInvalidCharacter_BetweenTwoValidCharacters() {
        String validCharacters = "~!@#$%^&*()_+1234567890-=QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm{}|[]" +
                "\\:\";'<>?,./`";

        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length(); i++) {
            String invalidCharacter = String.valueOf(invalidCharactersToTest.charAt(i));

            for (int j = 0; j < validCharacters.length() - 1; j++) {
                boolean atBacktickCharacter = j == validCharacters.length() - 2;

                String validCharacter1 = String.valueOf(validCharacters.charAt(j));
                String validCharacter2 = String.valueOf(validCharacters.charAt(j + 1));

                String sourceString = validCharacter1 + invalidCharacter + validCharacter2;

                SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
                int expectedErrorCount = atBacktickCharacter ? 2 : 1;

                List<String> tokens = lexSingleStatementAsTokens(sourceString, 3, syntaxErrorReporter, expectedErrorCount);

                testToken(tokens.get(0), validCharacter1);
                testToken(tokens.get(1), invalidCharacter);
                testToken(tokens.get(2), validCharacter2);

                List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();

                testSyntaxError(syntaxErrors.get(0), location(0, 1), sourceString, "Invalid characters.");
                if (atBacktickCharacter) {
                    testSyntaxError(syntaxErrors.get(1), location(0, 2), sourceString, "Missing closing backtick quotation `.");
                }
            }
        }
    }

    @Test
    @Order(6)
    public void testLexer_StringTokens_InvalidCharacters_TwoInvalidCharactersAroundAValidCharacter() {
        String validCharacters = "~!@#$%^&*()_+1234567890-=QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm{}|[]" +
                "\\:\";'<>?,./`";

        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length() - 1; i++) {
            String invalidCharacter1 = String.valueOf(invalidCharactersToTest.charAt(i));
            String invalidCharacter2 = String.valueOf(invalidCharactersToTest.charAt(i + 1));

            for (int j = 0; j < validCharacters.length(); j++) {
                boolean atBacktickCharacter = j == validCharacters.length() - 1;

                String validCharacter = String.valueOf(validCharacters.charAt(j));

                String sourceString = invalidCharacter1 + validCharacter + invalidCharacter2;
                SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
                int expectedTokenCount = atBacktickCharacter ? 2 : 3;

                List<String> tokens = lexSingleStatementAsTokens(sourceString, expectedTokenCount, syntaxErrorReporter, 2);
                testToken(tokens.get(0), invalidCharacter1);

                if (atBacktickCharacter) {
                    testToken(tokens.get(1), validCharacter + invalidCharacter2);
                } else {
                    testToken(tokens.get(1), validCharacter);
                    testToken(tokens.get(2), invalidCharacter2);
                }

                List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();

                testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Invalid characters.");
                if (atBacktickCharacter) {
                    testSyntaxError(syntaxErrors.get(1), location(0, 1), sourceString, "Missing closing backtick quotation `.");
                } else {
                    testSyntaxError(syntaxErrors.get(1), location(0, 2), sourceString, "Invalid characters.");
                }
            }
        }
    }

    @Test
    @Order(7)
    public void testLexer_StringTokens_InvalidCharacters_SingleLargeErrorToken() {
        String sourceString = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈ç√∫˜µ≤≥÷¸˛Ç" +
                "◊ı˜Â¯˘¿";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<String> tokens = lexSingleStatementAsTokens(sourceString, 1, syntaxErrorReporter, 1);
        testToken(tokens.get(0), sourceString);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Invalid characters.");
    }

    @Test
    @Order(8)
    public void testLexer_StringTokens_InvalidCharacters_TestMultipleLargeTokens() {
        String invalidToken1 = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°";
        String invalidToken3 = "·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´";
        String invalidToken5 = "ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æ";
        String invalidToken7 = "ÅÍÎÏ˝ÓÔÒÚÆΩ≈ç√";
        String invalidToken9 = "∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        String validToken2 = "\t";
        String validToken4 = "_foo_bar_";
        String validToken6 = " ";
        String validToken8 = "921.7D";

        String sourceString = String.join("", invalidToken1, validToken2, invalidToken3, validToken4, invalidToken5,
                validToken6, invalidToken7, validToken8, invalidToken9);

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        List<String> tokens = lexSingleStatementAsTokens(sourceString, 9, syntaxErrorReporter, 5);

        testToken(tokens.get(0), invalidToken1);
        testToken(tokens.get(1),   validToken2);
        testToken(tokens.get(2), invalidToken3);
        testToken(tokens.get(3),   validToken4);
        testToken(tokens.get(4), invalidToken5);
        testToken(tokens.get(5),   validToken6);
        testToken(tokens.get(6), invalidToken7);
        testToken(tokens.get(7),   validToken8);
        testToken(tokens.get(8), invalidToken9);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        String expectedErrorMessage = "Invalid characters.";

        int column1 = 0;
        int column2 = invalidToken1.length() + validToken2.length();
        int column3 = column2 + invalidToken3.length() + validToken4.length();
        int column4 = column3 + invalidToken5.length() + validToken6.length();
        int column5 = column4 + invalidToken7.length() + validToken8.length();

        testSyntaxError(syntaxErrors.get(0), location(0, column1), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(1), location(0, column2), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(2), location(0, column3), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(3), location(0, column4), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(4), location(0, column5), sourceString, expectedErrorMessage);
    }
}
