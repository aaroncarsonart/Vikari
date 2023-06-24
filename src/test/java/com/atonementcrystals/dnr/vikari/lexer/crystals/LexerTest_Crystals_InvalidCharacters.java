package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;

public class LexerTest_Crystals_InvalidCharacters {

    @Test
    @Order(1)
    public void testLexer_Crystals_InvalidCharacters_SingleInvalidCharacters() {
        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length(); i++) {
            String sourceString = String.valueOf(invalidCharactersToTest.charAt(i));

            SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
            lex(sourceString, 0, syntaxErrorReporter, 1);
        }
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_InvalidCharacters_SingleLargeErrorToken() {
        String sourceString = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈ç√∫˜µ≤≥÷¸˛Ç" +
                "◊ı˜Â¯˘¿";

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lex(sourceString, 0, syntaxErrorReporter, 1);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Invalid characters.");
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_InvalidCharacters_TestMultipleLargeTokens() {
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
        List<AtonementCrystal> crystals = lexSingleStatement(sourceString, 2, syntaxErrorReporter, 5);

        int column1 = 0;
        int column2 = invalidToken1.length();
        int column3 = column2 +   validToken2.length();
        int column4 = column3 + invalidToken3.length();
        int column5 = column4 +   validToken4.length();
        int column6 = column5 + invalidToken5.length();
        int column7 = column6 +   validToken6.length();
        int column8 = column7 + invalidToken7.length();
        int column9 = column8 +   validToken8.length();

        testCrystal(crystals.get(0), ReferenceCrystal.class, validToken4, location(0, column4));
        testCrystal(crystals.get(1), DoubleCrystal.class, validToken8, location(0, column8));

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        String expectedErrorMessage = "Invalid characters.";

        testSyntaxError(syntaxErrors.get(0), location(0, column1), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(1), location(0, column3), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(2), location(0, column5), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(3), location(0, column7), sourceString, expectedErrorMessage);
        testSyntaxError(syntaxErrors.get(4), location(0, column9), sourceString, expectedErrorMessage);
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_InvalidCharacters_Crystals_AsCharacterLiteral() {
        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length(); i++) {
            char invalidCharacter = invalidCharactersToTest.charAt(i);
            String sourceString = String.format("`%c`", invalidCharacter);

            List<AtonementCrystal> crystals = lexSingleStatement(sourceString, 1);

            // TODO: Change expectation to a character literal crystal once they are supported by Vikari.
            testCrystal(crystals.get(0), ReferenceCrystal.class, sourceString, location(0, 0));
        }
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_InvalidCharacters_AsBacktickQuotedIdentifiers() {
        String token1 = "`¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°`";
        String token3 = "`·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´`";
        String token5 = "`ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æ`";
        String token7 = "`ÅÍÎÏ˝ÓÔÒÚÆΩ≈ç√`";
        String token9 = "`∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿`";

        String token2 = "<<";
        String token4 = "+";
        String token6 = "-";
        String token8 = "/";

        String sourceString = String.join("", token1, token2, token3, token4, token5, token6, token7, token8, token9);

        List<AtonementCrystal> crystals = lexSingleStatement(sourceString, 9);

        int column1 = 0;
        int column2 = token1.length();
        int column3 = column2 + token2.length();
        int column4 = column3 + token5.length();
        int column5 = column4 + token4.length();
        int column6 = column5 + token5.length();
        int column7 = column6 + token6.length();
        int column8 = column7 + token7.length();
        int column9 = column8 + token8.length();

        testCrystal(crystals.get(0), ReferenceCrystal.class, token1, location(0, column1));
        testCrystal(crystals.get(1), LeftAssignmentOperatorCrystal.class, token2, location(0, column2));
        testCrystal(crystals.get(2), ReferenceCrystal.class, token3, location(0, column3));
        testCrystal(crystals.get(3), AddOperatorCrystal.class, token4, location(0, column4));
        testCrystal(crystals.get(4), ReferenceCrystal.class, token5, location(0, column5));
        testCrystal(crystals.get(5), SubtractOperatorCrystal.class, token6, location(0, column6));
        testCrystal(crystals.get(6), ReferenceCrystal.class, token7, location(0, column7));
        testCrystal(crystals.get(7), LeftDivideOperatorCrystal.class, token8, location(0, column8));
        testCrystal(crystals.get(8), ReferenceCrystal.class, token9, location(0, column9));
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_InvalidCharacters_AsBacktickQuotedIdentifiers_MixedWithValidCharacters() {
        String token1 = "`abc ¡™£¢ def ∞§¶• ghi`";
        String token3 = "`·‚—± jkl œ∑´® mno †¥¨ˆ`";
        String token5 = "`ˇ123 Á¨ˆØ 456 ∏”’» 7890`";
        String token7 = "`ÅÍÎÏ XYZ ˝ÓÔ UVW ÒÚÆΩ`";
        String token9 = "`<>? ∫˜µ≤ ,./ ≥÷¸˛{[]\\ Ç◊ı˜`";

        String token2 = "<<";
        String token4 = "+";
        String token6 = "-";
        String token8 = "/";

        String sourceString = String.join("", token1, token2, token3, token4, token5, token6, token7, token8, token9);

        List<AtonementCrystal> crystals = lexSingleStatement(sourceString, 9);

        int column1 = 0;
        int column2 = token1.length();
        int column3 = column2 + token2.length();
        int column4 = column3 + token3.length();
        int column5 = column4 + token4.length();
        int column6 = column5 + token5.length();
        int column7 = column6 + token6.length();
        int column8 = column7 + token7.length();
        int column9 = column8 + token8.length();

        testCrystal(crystals.get(0), ReferenceCrystal.class, token1, location(0, column1));
        testCrystal(crystals.get(1), LeftAssignmentOperatorCrystal.class, token2, location(0, column2));
        testCrystal(crystals.get(2), ReferenceCrystal.class, token3, location(0, column3));
        testCrystal(crystals.get(3), AddOperatorCrystal.class, token4, location(0, column4));
        testCrystal(crystals.get(4), ReferenceCrystal.class, token5, location(0, column5));
        testCrystal(crystals.get(5), SubtractOperatorCrystal.class, token6, location(0, column6));
        testCrystal(crystals.get(6), ReferenceCrystal.class, token7, location(0, column7));
        testCrystal(crystals.get(7), LeftDivideOperatorCrystal.class, token8, location(0, column8));
        testCrystal(crystals.get(8), ReferenceCrystal.class, token9, location(0, column9));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_InvalidCharacters_Crystals_SingleCharacterAsStringLiteral() {
        String invalidCharactersToTest = "¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´‰ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æÅÍÎÏ˝ÓÔÒÚÆΩ≈" +
                "ç√∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿";

        for (int i = 0; i < invalidCharactersToTest.length(); i++) {
            char invalidCharacter = invalidCharactersToTest.charAt(i);
            String sourceString = String.format("``%c``", invalidCharacter);

            List<AtonementCrystal> crystals = lexSingleStatement(sourceString, 1);
            testCrystal(crystals.get(0), StringLiteralCrystal.class, sourceString, location(0, 0));
        }
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_InvalidCharacters_AsStringLiterals() {
        String token1 = "``¡™£¢∞§¶•ªº–≠⁄€‹›ﬁﬂ‡°``";
        String token3 = "``·‚—±œ∑´®†¥¨ˆøπ“‘«Œ„´``";
        String token5 = "``ˇÁ¨ˆØ∏”’»åß∂ƒ©˙∆˚¬…æ``";
        String token7 = "``ÅÍÎÏ˝ÓÔÒÚÆΩ≈ç√``";
        String token9 = "``∫˜µ≤≥÷¸˛Ç◊ı˜Â¯˘¿``";

        String token2 = "+";
        String token4 = "+";
        String token6 = "+";
        String token8 = "+";

        String sourceString = String.join("", token1, token2, token3, token4, token5, token6, token7, token8, token9);

        List<AtonementCrystal> crystals = lexSingleStatement(sourceString, 9);

        int column1 = 0;
        int column2 = token1.length();
        int column3 = column2 + token2.length();
        int column4 = column3 + token3.length();
        int column5 = column4 + token4.length();
        int column6 = column5 + token5.length();
        int column7 = column6 + token6.length();
        int column8 = column7 + token7.length();
        int column9 = column8 + token8.length();

        testCrystal(crystals.get(0), StringLiteralCrystal.class, token1, location(0, column1));
        testCrystal(crystals.get(1), AddOperatorCrystal.class, token2, location(0, column2));
        testCrystal(crystals.get(2), StringLiteralCrystal.class, token3, location(0, column3));
        testCrystal(crystals.get(3), AddOperatorCrystal.class, token4, location(0, column4));
        testCrystal(crystals.get(4), StringLiteralCrystal.class, token5, location(0, column5));
        testCrystal(crystals.get(5), AddOperatorCrystal.class, token6, location(0, column6));
        testCrystal(crystals.get(6), StringLiteralCrystal.class, token7, location(0, column7));
        testCrystal(crystals.get(7), AddOperatorCrystal.class, token8, location(0, column8));
        testCrystal(crystals.get(8), StringLiteralCrystal.class, token9, location(0, column9));
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_InvalidCharacters_AsStringLiterals_MixedWithValidCharacters() {
        String token1 = "``abc ¡™£¢ def ∞§¶• ghi``";
        String token3 = "``·‚—± jkl œ∑´® mno †¥¨ˆ``";
        String token5 = "``ˇ123 Á¨ˆØ 456 ∏”’» 7890``";
        String token7 = "``ÅÍÎÏ XYZ ˝ÓÔ UVW ÒÚÆΩ``";
        String token9 = "``<>? ∫˜µ≤ ,./ ≥÷¸˛{[]\\ Ç◊ı˜``";

        String token2 = "+";
        String token4 = "+";
        String token6 = "+";
        String token8 = "+";

        String sourceString = String.join("", token1, token2, token3, token4, token5, token6, token7, token8, token9);

        List<AtonementCrystal> crystals = lexSingleStatement(sourceString, 9);

        int column1 = 0;
        int column2 = token1.length();
        int column3 = column2 + token2.length();
        int column4 = column3 + token3.length();
        int column5 = column4 + token4.length();
        int column6 = column5 + token5.length();
        int column7 = column6 + token6.length();
        int column8 = column7 + token7.length();
        int column9 = column8 + token8.length();

        testCrystal(crystals.get(0), StringLiteralCrystal.class, token1, location(0, column1));
        testCrystal(crystals.get(1), AddOperatorCrystal.class, token2, location(0, column2));
        testCrystal(crystals.get(2), StringLiteralCrystal.class, token3, location(0, column3));
        testCrystal(crystals.get(3), AddOperatorCrystal.class, token4, location(0, column4));
        testCrystal(crystals.get(4), StringLiteralCrystal.class, token5, location(0, column5));
        testCrystal(crystals.get(5), AddOperatorCrystal.class, token6, location(0, column6));
        testCrystal(crystals.get(6), StringLiteralCrystal.class, token7, location(0, column7));
        testCrystal(crystals.get(7), AddOperatorCrystal.class, token8, location(0, column8));
        testCrystal(crystals.get(8), StringLiteralCrystal.class, token9, location(0, column9));
    }
}
