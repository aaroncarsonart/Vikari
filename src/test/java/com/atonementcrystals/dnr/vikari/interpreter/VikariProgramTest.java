package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.Main;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class VikariProgramTest {

    private final PrintStream originalOut = System.out;
    private final ByteArrayOutputStream testOut = new ByteArrayOutputStream();

    @BeforeEach
    public void setupPrintStream() {
        System.setOut(new PrintStream(testOut));
    }

    @AfterEach
    public void restorePrintStream() {
        System.setOut(originalOut);
    }

    private void runVikariProgram(String sourceString) {
        Main.runSourceString(sourceString, Phase.EXECUTE, null, null);
    }

    private void assertOutput(String expectedOutput) {
        String actualOutput = testOut.toString();
        assertEquals(expectedOutput, actualOutput, "Unexpected output of print statements.");
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
        assertOutput("2");
    }

    @Test
    @Order(3)
    public void testVariableExpressions_Declaration_NullInitializer() {
        String sourceString = """
                a:Integer
                :a
                """;
        runVikariProgram(sourceString);
        assertOutput("{0}");
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
        assertOutput("3.142857142857142857142857142857143");
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
        assertOutput("2\n4.0\n");
    }
}
