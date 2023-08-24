package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeHierarchy;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;

import java.math.BigDecimal;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static com.atonementcrystals.dnr.vikari.TestUtils.testNumberCrystal;
import static org.junit.jupiter.api.Assertions.*;

public class TreeWalkInterpreterTest_Base {
    private final AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();
    private AtonementField currentEnvironment;

    public void lexParseAndInterpret(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLineFromCache);

        parser.setGlobalAtonementField(globalAtonementField);
        interpreter.setGlobalAtonementField(globalAtonementField);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);
        assertNoSyntaxErrors(syntaxErrorReporter);
        interpreter.interpret(null, parsedStatements);

        currentEnvironment = interpreter.getCurrentEnvironment();
    }

    public void testVariable(String identifier, VikariType declaredType) {
        testVariable(identifier, declaredType, VikariType.NULL, 0);
    }

    public void testVariable(String identifier, VikariType declaredType, VikariType instantiatedType,
                              Object expectedValue) {

        assertTrue(currentEnvironment.isDefined(identifier), "Expected variable to be defined.");
        AtonementCrystal variable = currentEnvironment.get(identifier);

        // 1: Check types.
        assertEquals(declaredType.getTypeCrystal(), variable.getDeclaredType(), "Unexpected declared type.");

        if (instantiatedType != VikariType.NULL) {
            assertEquals(instantiatedType.getTypeCrystal(), variable.getInstantiatedType(), "Unexpected instantiated type.");
        } else {
            TypeCrystal nullType = TypeHierarchy.getNullTypeFor(declaredType);
            assertEquals(nullType, variable.getInstantiatedType(), "Unexpected null instantiated type.");
        }

        // 2: Check value.
        if (instantiatedType == VikariType.NULL) {
            assertEquals(NullCrystal.class, variable.getClass(), "Unexpected type for declaration result.");
            NullCrystal nullCrystal = (NullCrystal) variable;

            int expectedLength = (Integer) expectedValue;
            int actualLength = nullCrystal.getLength();
            assertEquals(expectedLength, actualLength, "Unexpected length for null crystal.");
        }

        else if (variable instanceof ValueCrystal<?> valueCrystal) {
            if (expectedValue instanceof BigDecimal) {
                testNumberCrystal(variable, expectedValue, BigDecimalCrystal.class);
            } else {
                Object actualValue = valueCrystal.getValue();
                assertEquals(expectedValue, actualValue, "Unexpected value.");
            }
        } else {
            TypeCrystal typeCrystal = variable.getInstantiatedType();
            if (typeCrystal == null) {
                typeCrystal = variable.getDeclaredType();
            }
            fail("Malformed test. Unhandled type in declaration: " + typeCrystal);
        }
    }

    public void testNullVariable(String identifier, VikariType declaredType, int expectedLength) {
        testNullVariable(identifier, declaredType, expectedLength, currentEnvironment);
    }

    public void testNullVariable(String identifier, VikariType declaredType, int expectedLength,
                                        AtonementField environment) {

        assertTrue(environment.isDefined(identifier), "Expected variable to be defined.");
        AtonementCrystal variable = environment.get(identifier);

        assertEquals(declaredType.getTypeCrystal(), variable.getDeclaredType(), "Unexpected declared type.");

        TypeCrystal expectedInstantiatedType = TypeHierarchy.getNullTypeFor(declaredType);
        assertEquals(expectedInstantiatedType, variable.getInstantiatedType(), "Unexpected instantiated type.");

        assertEquals(NullCrystal.class, variable.getClass(), "Unexpected type for null variable.");
        NullCrystal nullCrystal = (NullCrystal) variable;

        assertEquals(expectedLength, nullCrystal.getLength(), "Unexpected length for null crystal.");
    }

    /**
     * Execute a single Vikari expression statement, and return the AtonementCrystal returned by its evaluation.
     * @param sourceString The Vikari source code string to execute.
     * @return The AtonementCrystal returned by evaluating this expression statement.
     */
    public AtonementCrystal testVikariExpression(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);
        assertEquals(1, parsedStatements.size(), "Unexpected number of statements");

        Statement statement = parsedStatements.get(0);
        return interpreter.execute(statement);
    }

    /**
     * Test a single Vikari arithmetic expression statement returning a numeric value.
     * @param sourceString The Vikari source code string to execute.
     * @param expectedValue The expected value for the result.
     * @param expectedClass The expected type of the result.
     */
    public void testArithmeticExpression(String sourceString, Object expectedValue,
                                         Class<? extends NumberCrystal<?>> expectedClass) {
        AtonementCrystal crystal = testVikariExpression(sourceString);
        testNumberCrystal(crystal, expectedValue, expectedClass);
    }

    /**
     * Test a single Vikari expression statement returning a BigDecimal value.
     * Compares the numbers with equals() and not compareTo() to assert equality of scale parameters.
     * @param sourceString The Vikari source code string to execute.
     * @param expectedValue The expected BigDecimal value for the crystal returned by this expression.
     */
    public void testBigDecimalScale(String sourceString, BigDecimal expectedValue) {
        AtonementCrystal crystal = testVikariExpression(sourceString);

        if (crystal instanceof BigDecimalCrystal bigDecimalCrystal) {
            BigDecimal actualValue = bigDecimalCrystal.getValue();
            assertEquals(expectedValue, actualValue, "Unexpected value for BigIntegerCrystal.");
        } else {
            String typeName = crystal.getClass().getSimpleName();
            fail ("Malformed test. Expected a BigDecimal, but instead the statement returned a " + typeName + ".");
        }
    }
}
