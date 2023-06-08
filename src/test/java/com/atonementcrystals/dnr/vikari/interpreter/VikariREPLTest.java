package com.atonementcrystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class VikariREPLTest {
    private VikariREPL repl;
    private final PrintStream originalOut = System.out;
    private final ByteArrayOutputStream testOut = new ByteArrayOutputStream();

    @BeforeEach
    public void setupPrintStream() {
        repl = new VikariREPL();
        System.setOut(new PrintStream(testOut));
    }

    @AfterEach
    public void restorePrintStream() {
        System.setOut(originalOut);
    }

    /**
     * Test the input string against the output of the REPL.
     * @param expectedOutput The string to test. A trailing newline may be omitted.
     */
    private void testOutput(String expectedOutput) {
        if (!expectedOutput.endsWith("\n"))  {
            expectedOutput += "\n";
        }
        assertEquals(expectedOutput, testOut.toString(), "Unexpected REPL output.");

        // Clear the output buffer.
        testOut.reset();
    }

    /**
     * Test that the output of the REPL is completely empty.
     */
    private void testThatOutputIsBlank() {
        assertEquals("", testOut.toString(), "Unexpected REPL output.");
    }

    @Test
    @Order(1)
    public void testDeclaration() {
        repl.lexParseAndInterpret("foo << 2");
        testOutput("foo:Integer = 2");
    }

    @Test
    @Order(2)
    public void testDeclaration_NullInitializer() {
        repl.lexParseAndInterpret("foo");
        testOutput("foo:AtonementCrystal = {0}");
    }

    @Test
    @Order(3)
    public void testDeclaration_AndVariableInInitializerExpression_AcrossTwoLines() {
        repl.lexParseAndInterpret("foo << 2");
        repl.lexParseAndInterpret("bar << foo + 2");

        String expectedOutput = """
            foo:Integer = 2
            bar:Integer = 4
            """;

        testOutput(expectedOutput);
    }

    @Test
    @Order(4)
    public void testDeclaration_AndReAssignments() {
        repl.lexParseAndInterpret("foo << 2");
        repl.lexParseAndInterpret("foo << 4");
        repl.lexParseAndInterpret("6 >> foo");

        String expectedOutput = """
            foo:Integer = 2
            foo:Integer = 4
            foo:Integer = 6
            """;

        testOutput(expectedOutput);
    }

    @Test
    @Order(5)
    public void testDeclaration_MultipleAssignments() {
        // Test declarations.
        repl.lexParseAndInterpret("a, b, c, d");

        String expectedOutput = """
            a:AtonementCrystal = {0}
            b:AtonementCrystal = {0}
            c:AtonementCrystal = {0}
            d:AtonementCrystal = {0}
            """;

        testOutput(expectedOutput);

        // Test complex assignment on one line.
        repl.lexParseAndInterpret("a << [b << 7] * [c << 6L] / [d << 22F]");

        expectedOutput = """
            a:Float = 1.9090909
            b:Integer = 7
            c:Long = 6
            d:Float = 22.0
            """;

        testOutput(expectedOutput);
    }

    @Test
    @Order(6)
    public void testDeclaration_MultipleAssignments_SameVariable() {
        repl.lexParseAndInterpret("a");
        testOutput("a:AtonementCrystal = {0}");

        repl.lexParseAndInterpret("a << [a << 7] * [a << 6L] / [a << 22F]");
        testOutput("a:Float = 1.9090909");
    }

    @Test
    @Order(7)
    public void testExpressions() {
        repl.lexParseAndInterpret("2");
        testOutput("2");

        repl.lexParseAndInterpret("2 + 4");
        testOutput("6");

        repl.lexParseAndInterpret("5 - 7");
        testOutput("-2");

        repl.lexParseAndInterpret("13 * 22");
        testOutput("286");

        repl.lexParseAndInterpret("22 / 7");
        testOutput("3");

        repl.lexParseAndInterpret("7 \\ 22");
        testOutput("3");

        repl.lexParseAndInterpret("5 + [7 - 3] * 4 / [9 + 2]");
        testOutput("6");
    }

    @Test
    @Order(8)
    public void testExpression_ContainingAssignment() {
        repl.lexParseAndInterpret("foo:Integer");
        testOutput("foo:Integer = {0}");

        repl.lexParseAndInterpret("6 + [foo << 2]");

        String expectedOutput = """
            8
            foo:Integer = 2
            """;

        testOutput(expectedOutput);
    }

    @Test
    @Order(9)
    public void testPrintStatement() {
        // Empty PrintStatement
        repl.lexParseAndInterpret(":");
        testOutput("");

        // With newline
        repl.lexParseAndInterpret(":2:4:6:");
        testOutput("246");

        // Without newline
        repl.lexParseAndInterpret(":1:2:3");
        testOutput("123");

        // Containing assignment
        repl.lexParseAndInterpret("foo");
        testOutput("foo:AtonementCrystal = {0}");

        repl.lexParseAndInterpret(":foo << 2:4:6:");

        String expectedOutput = """
            246
            foo:Integer = 2
            """;

        testOutput(expectedOutput);
    }

    /**
     * Test that the REPL output matches expectations when toggling !verbose.
     */
    @Test
    @Order(10)
    public void testReplCommand_Verbose() {
        // Test with verbose mode disabled.
        repl.lexParseAndInterpret("!verbose");
        testOutput("Verbose output mode disabled.");

        repl.lexParseAndInterpret("foo:Integer << 2");
        testThatOutputIsBlank();

        repl.lexParseAndInterpret("foo << 4");
        testThatOutputIsBlank();

        repl.lexParseAndInterpret("foo << 6");
        testThatOutputIsBlank();

        repl.lexParseAndInterpret("22 / 7");
        testThatOutputIsBlank();

        repl.lexParseAndInterpret(":foo");
        testOutput("6");

        // Test with verbose mode enabled.
        repl.lexParseAndInterpret("!verbose");
        testOutput("Verbose output mode enabled.");

        repl.lexParseAndInterpret("foo << 4");
        testOutput("foo:Integer = 4");

        repl.lexParseAndInterpret("foo << 6");
        testOutput("foo:Integer = 6");

        repl.lexParseAndInterpret("22 / 7");
        testOutput("3");

        repl.lexParseAndInterpret(":foo");
        testOutput("6");
    }

    /**
     * Test that the REPL state is cleared by !clear.
     * (So that variables can be redeclared with a different type.)
     */
    @Test
    @Order(11)
    public void testReplCommand_Clear() {
        repl.lexParseAndInterpret("foo:Long << 2L");
        testOutput("foo:Long = 2");

        repl.lexParseAndInterpret("!clear");

        repl.lexParseAndInterpret("foo:Double << 4D");
        testOutput("foo:Double = 4.0");
    }
}
