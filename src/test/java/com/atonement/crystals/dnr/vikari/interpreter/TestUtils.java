package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonement.crystals.dnr.vikari.error.SyntaxError;
import com.atonement.crystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonement.crystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Helper methods for Lexer, Parser, and Intepreter test cases.
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
        assertTrue(errorMessage.contains(partialErrorMessage), "Unexpected syntax error message.");
    }

    public static void assertNoSyntaxErrors(SyntaxErrorReporter syntaxErrorReporter) {
        if(syntaxErrorReporter.hasErrors()) {
            syntaxErrorReporter.reportErrors();
            fail("Expected no syntax errors for test case.");
        }
    }

    public static void testNumberCrystal(AtonementCrystal crystal, Object expectedValue, Class<? extends NumberCrystal> expectedClass) {
        Class<? extends AtonementCrystal> actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Unexpected type.");

        NumberCrystal numberCrystal = expectedClass.cast(crystal);
        Object actualValue = numberCrystal.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for NumberCrystal.");
    }
}
