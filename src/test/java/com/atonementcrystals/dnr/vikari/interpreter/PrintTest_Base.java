package com.atonementcrystals.dnr.vikari.interpreter;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

/**
 * Base class for testing assertions on content of print statements in Vikari.
 */
public abstract class PrintTest_Base {
    private static final String CARRIAGE_RETURN = "\r";
    private static final boolean SANITIZE_OUTPUT = System.lineSeparator().contains(CARRIAGE_RETURN);

    protected final PrintStream originalOut = System.out;
    protected final ByteArrayOutputStream testOut = new ByteArrayOutputStream();

    @BeforeEach
    public void setupPrintStream() {
        System.setOut(new PrintStream(testOut));
    }

    @AfterEach
    public void restorePrintStream() {
        System.setOut(originalOut);
    }

    /**
     * @return The error message used for {@link #testOutput(String)}.
     */
    protected String getTestOutputErrorMessage() {
        return "Unexpected output of print statement.";
    }

    protected void testOutput(String expectedOutput) {
        String actualOutput = testOut.toString();

        if (SANITIZE_OUTPUT) {
            actualOutput = actualOutput.replaceAll(CARRIAGE_RETURN, "");
        }

        assertEquals(expectedOutput, actualOutput, getTestOutputErrorMessage());
        testOut.reset();
    }
}
