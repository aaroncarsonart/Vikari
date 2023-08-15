package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeHierarchy;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
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
            Object actualvalue = valueCrystal.getValue();
            assertEquals(expectedValue, actualvalue, "Unexpected value.");
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
}
