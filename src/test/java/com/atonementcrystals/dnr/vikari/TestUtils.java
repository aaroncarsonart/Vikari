package com.atonementcrystals.dnr.vikari;

import com.atonementcrystals.dnr.vikari.core.crystal.literal.MultiLineStringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Helper methods for Lexer, Parser, and Interpreter test cases.
 */
public class TestUtils {
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
        assertTrue(errorMessage.contains(partialErrorMessage), "Expected the syntax error message to contain \"" +
                partialErrorMessage + "\", but instead the error message was \"" + errorMessage + "\".");
    }

    public static void assertNoSyntaxErrors(SyntaxErrorReporter syntaxErrorReporter) {
        if(syntaxErrorReporter.hasErrors()) {
            syntaxErrorReporter.reportErrors();
            fail("Expected no syntax errors for test case.");
        }
    }

    public static void assertSyntaxErrors(SyntaxErrorReporter syntaxErrorReporter, int expectedErrorCount) {
        int actualErrorCount = syntaxErrorReporter.getSyntaxErrors().size();
        assertEquals(expectedErrorCount, actualErrorCount, "Unexpected number of syntax errors.");
    }

    public static void testNumberCrystal(AtonementCrystal crystal, Object expectedValue, Class<? extends NumberCrystal> expectedClass) {
        Class<? extends AtonementCrystal> actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Unexpected type.");

        NumberCrystal numberCrystal = expectedClass.cast(crystal);
        Object actualValue = numberCrystal.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for NumberCrystal.");
    }

    public static AtonementCrystal testCrystal(AtonementCrystal crystal, Class<? extends AtonementCrystal> expectedClass,
                                   String expectedIdentifier, CoordinatePair expectedLocation) {

        assertEquals(expectedClass, crystal.getClass(), "Unexpected crystal type.");
        assertEquals(expectedIdentifier, crystal.getIdentifier(), "Unexpected Identifier.");
        assertEquals(expectedLocation, crystal.getCoordinates(), "Unexpected location.");

        return crystal;
    }

    public static MultiLineStringLiteralCrystal testMultiLineStringLiteral(AtonementCrystal crystal,
                                                                           String expectedIdentifier,
                                                                           CoordinatePair expectedLocation) {

        testCrystal(crystal, MultiLineStringLiteralCrystal.class, expectedIdentifier, expectedLocation);
        return (MultiLineStringLiteralCrystal) crystal;
    }

    public static void testLinkage(MultiLineStringLiteralCrystal... crystals) {
        if (crystals.length == 0) {
            fail("Malformed test. Expected at least 1 multi-line crystal for testLinkage().");
        }

        for (int i = 0; i < crystals.length; i++) {
            MultiLineStringLiteralCrystal current = crystals[i];
            MultiLineStringLiteralCrystal next = null;
            if (i + 1 < crystals.length) {
                next = crystals[i + 1];
            }
            assertEquals(next, current.getNext(), "Expected multi-line crystal " + (i + 1) + " to be linked to " +
                    ((next == null) ? "null." : "multi-line crystal " + (i + 2) + "."));
        }
    }

    public static CoordinatePair location(int row, int column) {
        return new CoordinatePair(row, column);
    }
}
