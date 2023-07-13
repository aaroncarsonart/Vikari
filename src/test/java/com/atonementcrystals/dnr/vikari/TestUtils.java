package com.atonementcrystals.dnr.vikari;

import com.atonementcrystals.dnr.vikari.core.crystal.literal.MultiLineStringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.error.CompilationWarning;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Helper methods for Lexer, Parser, and Interpreter test cases.
 */
public class TestUtils {
    public static void testSyntaxError(VikariError syntaxError, CoordinatePair expectedLocation, String expectedLine,
                                       String partialErrorMessage) {
        testVikariError(syntaxError, SyntaxError.class, expectedLocation, expectedLine, partialErrorMessage);
    }

    public static void testWarning(VikariError compilationWarning, CoordinatePair expectedLocation,
                                   String expectedLine, String partialErrorMessage) {
        testVikariError(compilationWarning, CompilationWarning.class, expectedLocation, expectedLine, partialErrorMessage);
    }

    public static void testVikariError(VikariError vikariError, Class<? extends VikariError> expectedType,
                                       CoordinatePair expectedLocation, String expectedLine,
                                       String partialVikariErrorMessage) {

        Class<? extends VikariError> actualType = vikariError.getClass();
        assertEquals(expectedType, actualType, "Unexpected type for VikariError.");

        File expectedFile = null;
        File actualFile = vikariError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair actualLocation = vikariError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String actualLine = vikariError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String vikariErrorMessage = vikariError.getMessage();
        assertTrue(vikariErrorMessage.contains(partialVikariErrorMessage), "Expected the message to contain \"" +
                partialVikariErrorMessage + "\", but instead the message was \"" + vikariErrorMessage + "\".");
    }

    public static void assertNoSyntaxErrors(SyntaxErrorReporter syntaxErrorReporter) {
        if (syntaxErrorReporter.hasErrors()) {
            syntaxErrorReporter.reportSyntaxErrors();
            fail("Expected no syntax errors for test case.");
        }
    }

    public static void assertNoWarnings(SyntaxErrorReporter syntaxErrorReporter) {
        if (syntaxErrorReporter.hasWarnings()) {
            syntaxErrorReporter.reportWarnings();
            fail("Expected no compilation warnings for test case.");
        }
    }

    public static void assertSyntaxErrors(SyntaxErrorReporter syntaxErrorReporter, int expectedErrorCount) {
        int actualErrorCount = syntaxErrorReporter.getSyntaxErrors().size();
        assertEquals(expectedErrorCount, actualErrorCount, "Unexpected number of syntax errors.");
    }

    public static void assertWarnings(SyntaxErrorReporter syntaxErrorReporter, int expectedWarningCount) {
        int actualWarningCountCount = syntaxErrorReporter.getCompilationWarnings().size();
        assertEquals(expectedWarningCount, actualWarningCountCount, "Unexpected number of compilation warnings.");
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
