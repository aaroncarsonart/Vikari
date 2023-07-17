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
        if (!expectedOutput.isEmpty() && !expectedOutput.endsWith("\n"))  {
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
        testOutput("\n");

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

    @Test
    @Order(12)
    public void testComment() {
        repl.lexParseAndInterpret("~:Comment.:~");
        testOutput("");
    }

    @Test
    @Order(13)
    public void testComment_WithStatement() {
        repl.lexParseAndInterpret("~:Comment.:~ 5 + 3");
        testOutput("8");
    }

    @Test
    @Order(14)
    public void testMultiLineComment() {
        repl.lexParseAndInterpret("~:Multi-line\ncomment.:~");
        testOutput("");
    }

    @Test
    @Order(15)
    public void testMultiLineComment_WithStatement() {
        repl.lexParseAndInterpret(" 5 + ~:Multi-line\ncomment.:~ 3");
        testOutput("8");
    }

    @Test
    @Order(16)
    public void testSyntaxError_Basic() {
        repl.lexParseAndInterpret("+5");
        String expectedErrorReport = """
                    ^
            Error: Expected expression.\n""";
        testOutput(expectedErrorReport);
    }

    @Test
    @Order(17)
    public void testSyntaxError_MultipleErrorsOnSameLine() {
        repl.lexParseAndInterpret("2 + foo -");
        String expectedErrorReport = """
            <repl>:1:5:
                2 + foo -
                    ^
                Error: Undefined variable reference.

            <repl>:1:9:
                2 + foo -
                        ^
                Error: Expected expression.\n\n""";
        testOutput(expectedErrorReport);
    }

    @Test
    @Order(18)
    public void testSyntaxError_MultiLineStatement_ErrorOnFirstLine() {
        repl.lexParseAndInterpret("5 + ~:\n:~");
        String expectedErrorReport = """
            <repl>:1:3:
                5 + ~:
                  ^
                Error: Expected expression.\n\n""";
        testOutput(expectedErrorReport);
    }

    @Test
    @Order(19)
    public void testSyntaxError_MultiLineStatement_ErrorOnMiddleLine() {
        repl.lexParseAndInterpret("~:\n:~ 5 + ~:\n:~");
        String expectedErrorReport = """
            <repl>:2:6:
                :~ 5 + ~:
                     ^
                Error: Expected expression.\n\n""";
        testOutput(expectedErrorReport);
    }

    @Test
    @Order(20)
    public void testSyntaxError_MultiLineStatement_ErrorOnLastLine() {
        repl.lexParseAndInterpret("~:\n:~ ~:\n:~ 5 +");
        String expectedErrorReport = """
                         ^
            Error: Expected expression.\n""";
        testOutput(expectedErrorReport);
    }

    @Test
    @Order(21)
    public void testSyntaxError_MultiLineStatement_ErrorsOnLineAfterFirst() {
        repl.lexParseAndInterpret("5 - 2");
        testOutput("3");

        repl.lexParseAndInterpret("2 + foo ~:\n:~ * 4 -");
        String expectedErrorReport = """
            <repl>:2:5:
                2 + foo ~:
                    ^
                Error: Undefined variable reference.

            <repl>:3:8:
                :~ * 4 -
                       ^
                Error: Expected expression.\n\n""";
        testOutput(expectedErrorReport);
    }

    @Test
    @Order(22)
    public void testSyntaxError_MultipleErrors_AfterMultiLineStatementWithErrors() {
        repl.lexParseAndInterpret("+5 ~:\n:~");
        String expectedErrorReport = """
            <repl>:1:1:
                +5 ~:
                ^
                Error: Expected expression.\n\n""";
        testOutput(expectedErrorReport);

        repl.lexParseAndInterpret("2 + foo -");
        expectedErrorReport = """
            <repl>:1:5:
                2 + foo -
                    ^
                Error: Undefined variable reference.

            <repl>:1:9:
                2 + foo -
                        ^
                Error: Expected expression.\n\n""";
        testOutput(expectedErrorReport);
    }

    @Test
    @Order(23)
    public void testSyntaxError_VariableDeclarationWithExtraTokens_TestThatVariableCanBeRedefined() {
        repl.lexParseAndInterpret("foo -");
        String expectedErrorReport = """
                        ^
            Error: Unexpected token(s) in variable declaration statement.\n""";
        testOutput(expectedErrorReport);

        repl.lexParseAndInterpret("foo");
        testOutput("foo:AtonementCrystal = {0}");
    }

    @Test
    @Order(24)
    public void testSyntaxError_VariableIsAlreadyDefined() {
        // 1. Define a variable.
        repl.lexParseAndInterpret("a");
        testOutput("a:AtonementCrystal = {0}");

        // 2. Attempt to redefine it.
        repl.lexParseAndInterpret("a:Integer");
        String expectedOutput = """
                    ^
            Error: Variable is already defined.""";
        testOutput(expectedOutput);
    }

    @Test
    @Order(25)
    public void testWarning_OnlyLineContinuations() {
        repl.setWarningsEnabled(true);
        repl.lexParseAndInterpret("~");
        String expectedOutput = """
                    ^
            Warning: Statement contains only line continuations.
            """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(26)
    public void testWarning_OnFirstLine_LineContinuation_AtStartOfStatement() {
        repl.setWarningsEnabled(true);
        repl.lexParseAndInterpret("~\na");
        String expectedOutput = """
            <repl>:1:1:
                ~
                ^
                Warning: Unnecessary line continuation at start of statement.

            """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(27)
    public void testWarning_TwoWarnings_LineContinuation_AtStartAndEndOfStatement() {
        repl.setWarningsEnabled(true);
        repl.lexParseAndInterpret("~\na:Integer~\n");
        String expectedOutput = """
            <repl>:1:1:
                ~
                ^
                Warning: Unnecessary line continuation at start of statement.

            <repl>:2:10:
                a:Integer~
                         ^
                Warning: Unnecessary line continuation at end of statement.

            """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(28)
    public void testWarning_AndSyntaxError_LineContinuationAndUndefinedVariable() {
        repl.setWarningsEnabled(true);
        repl.lexParseAndInterpret("[a + 5]~");
        String expectedOutput = """
            <repl>:1:2:
                [a + 5]~
                 ^
                Error: Undefined variable reference.

            <repl>:1:8:
                [a + 5]~
                       ^
                Warning: Unnecessary line continuation at end of statement.

            """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(29)
    public void testReplCommand_Warnings() {
        // 1. Execute a statement that would produce a warning.
        repl.lexParseAndInterpret("[2 + 5]~");
        testOutput("7");

        // 2. Enable warnings.
        repl.lexParseAndInterpret("!warnings");
        testOutput("Warnings enabled.");

        // 3. Execute the same statement again. Expect a warning report.
        repl.lexParseAndInterpret("[2 + 5]~");
        String expectedOutput = """
                           ^
            Warning: Unnecessary line continuation at end of statement.
            """;
        testOutput(expectedOutput);

        // 4. Disable warnings.
        repl.lexParseAndInterpret("!warnings");
        testOutput("Warnings disabled.");

        // 5. Execute the same statement a third time. Expect it to execute as normal.
        repl.lexParseAndInterpret("[2 + 5]~");
        testOutput("7");
    }

    @Test
    @Order(30)
    public void testSyntaxError_TypeResolver_EnsureResolverErrorsAreClearedBetweenCalls() {
        // 1. Declare a variable.
        repl.lexParseAndInterpret("a");
        testOutput("a:AtonementCrystal = {0}");

        // 2. Cause a ResolverError in the TypeResolver.
        repl.lexParseAndInterpret("[a + 2]");
        String expectedOutput = """
                     ^
            Error: Arithmetic expression expects a Number for operands.
            """;
        testOutput(expectedOutput);

        // 3. Execute another statement with the variable. Ensure the ResolverError has been cleared.
        repl.lexParseAndInterpret("a");
        testOutput("{0}");
    }
}
