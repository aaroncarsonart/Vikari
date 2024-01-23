package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.Main;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class VikariProgramTest extends PrintTest_Base {

    private void runVikariProgram(String sourceString) {
        Main.runSourceString(sourceString, Phase.EXECUTE, null, null, false);
    }

    private void runMain(String[] args) {
        Main.main(args);
    }

    @Override
    protected String getTestOutputErrorMessage() {
        return "Unexpected output of print statements.";
    }

    @Test
    @Order(1)
    public void testGlobalAtonementField() {
        VikariProgram vikariProgram = new VikariProgram();
        AtonementField globalAtonementField = vikariProgram.getGlobalAtonementField();

        for (VikariType vikariType : VikariType.LANG_TYPES) {
            String typeName = vikariType.getTypeCrystal().getTypeName();
            assertTrue(globalAtonementField.isDefined(typeName), "Expected a lang type to be defined for the global " +
                    "Atonement Field using its shortened name.");

            String fullyQualifiedTypeName = vikariType.getTypeCrystal().getFullyQualifiedTypeName();
            assertTrue(globalAtonementField.isDefined(fullyQualifiedTypeName), "Expected a lang type to be defined " +
                    "for the global Atonement Field using its fully-qualified names.");
        }
    }

    @Test
    @Order(2)
    public void testVariableExpressions_BasicDeclaration() {
        String sourceString = """
                a:Integer << 2
                :a
                """;
        runVikariProgram(sourceString);
        testOutput("2");
    }

    @Test
    @Order(3)
    public void testVariableExpressions_Declaration_NullInitializer() {
        String sourceString = """
                a:Integer
                :a
                """;
        runVikariProgram(sourceString);
        testOutput("null");
    }

    @Test
    @Order(4)
    public void testVariableExpressions_Declaration_WithAssignment() {
        String sourceString = """
                a:Number
                a << 22.0B
                :a / 7
                """;
        runVikariProgram(sourceString);
        testOutput("3.14285714285714285714285714285714");
    }

    @Test
    @Order(5)
    public void testVariableExpressions_MultipleAssignments() {
        String sourceString = """
                a:Number
                :a << 2:
                :4.0F >> a:
                """;
        runVikariProgram(sourceString);
        testOutput("2\n4.0\n");
    }

    // The following 8 tests test all combinations of the following conditions for VikariProgram:
    // ```!program.hasErrors() && (!warningsEnabled || !program.hasWarnings())```

    @Test
    @Order(6)
    public void testMain_NoErrors_WarningsFlagEnabled_LineContinuationWarning() {
        String sourceString = """
                ~
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-w", "-c", sourceString };
        runMain(args);

        String expectedOutput = """
                ---------------------
                Compilation Warnings:
                ---------------------
                <repl>:1:1:
                    ~
                    ^
                    Unnecessary line continuation at start of statement.

                """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(7)
    public void testMain_NoErrors_WarningsFlagEnabled_NoWarnings() {
        String sourceString = """
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-w", "-c", sourceString };
        runMain(args);
        testOutput("2\n");
    }

    @Test
    @Order(8)
    public void testMain_NoErrors_WarningsFlagDisabled_LineContinuationWarning() {
        String sourceString = """
                ~
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-c", sourceString };
        runMain(args);
        testOutput("2\n");
    }

    @Test
    @Order(9)
    public void testMain_NoErrors_WarningsFlagDisabled_NoWarnings() {
        String sourceString = """
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-c", sourceString };
        runMain(args);
        testOutput("2\n");
    }

    @Test
    @Order(10)
    public void testMain_WithErrors_WarningsFlagEnabled_LineContinuationWarning() {
        String sourceString = """
                +5
                ~
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-w", "-c", sourceString };
        runMain(args);

        String expectedOutput = """
                --------------
                Syntax Errors:
                --------------
                <repl>:1:1:
                    +5
                    ^
                    Expected expression.

                ---------------------
                Compilation Warnings:
                ---------------------
                <repl>:2:1:
                    ~
                    ^
                    Unnecessary line continuation at start of statement.

                """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(11)
    public void testMain_WithErrors_WarningsFlagEnabled_NoWarnings() {
        String sourceString = """
                +5
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-w", "-c", sourceString };
        runMain(args);

        String expectedOutput = """
                --------------
                Syntax Errors:
                --------------
                <repl>:1:1:
                    +5
                    ^
                    Expected expression.

                """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(12)
    public void testMain_WithErrors_WarningsFlagDisabled_LineContinuationWarning() {
        String sourceString = """
                +5
                ~
                foo:Integer << 2
                :foo:
                """;
        String[] args = {"-c", sourceString};
        runMain(args);

        String expectedOutput = """
                --------------
                Syntax Errors:
                --------------
                <repl>:1:1:
                    +5
                    ^
                    Expected expression.

                """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(13)
    public void testMain_WithErrors_WarningsFlagDisabled_NoWarnings() {
        String sourceString = """
                +5
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-c", sourceString };
        runMain(args);

        String expectedOutput = """
                --------------
                Syntax Errors:
                --------------
                <repl>:1:1:
                    +5
                    ^
                    Expected expression.

                """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(14)
    public void testMain_LexerOptionsWarning_LineContinuationWarning() {
        String sourceString = """
                ~
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-E", "w", "-c", sourceString };
        runMain(args);

        String expectedOutput = """
                ---------------------
                Compilation Warnings:
                ---------------------
                <repl>:1:1:
                    ~
                    ^
                    Unnecessary line continuation at start of statement.

                ---------------
                Program Output:
                ---------------
                2
                """;
        testOutput(expectedOutput);
    }

    @Test
    @Order(15)
    public void testMain_LexerOptionsWarning_NoWarnings() {
        String sourceString = """
                foo:Integer << 2
                :foo:
                """;
        String[] args = { "-E", "w", "-c", sourceString };
        runMain(args);
        testOutput("2\n");
    }
}
