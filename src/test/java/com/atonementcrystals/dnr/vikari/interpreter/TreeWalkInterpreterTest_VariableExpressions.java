package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.NullCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_VariableExpressions {
    private final AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();
    private AtonementField currentEnvironment;

    private void lexParseAndInterpret(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLine);

        parser.setGlobalAtonementField(globalAtonementField);
        interpreter.setGlobalAtonementField(globalAtonementField);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);
        interpreter.interpret(null, parsedStatements);

        currentEnvironment = interpreter.getCurrentEnvironment();
    }

    private void testVariable(String identifier, VikariType declaredType, VikariType instantiatedType,
                              Object expectedValue) {

        assertTrue(currentEnvironment.isDefined(identifier), "Expected variable to be defined.");
        AtonementCrystal variable = currentEnvironment.get(identifier);

        // 1: Check types.
        assertEquals(declaredType.getTypeCrystal(), variable.getDeclaredType(), "Unexpected declared type.");

        if (instantiatedType != null) {
            assertEquals(instantiatedType.getTypeCrystal(), variable.getInstantiatedType(), "Unexpected instantiated type.");
        } else {
            assertNull(variable.getInstantiatedType(), "Unexpected instantiated type.");
        }

        // 2: Check value.
        if (expectedValue == null) {
            assertEquals(NullCrystal.class, variable.getClass(), "Unexpected type for declaration result.");
            NullCrystal nullCrystal = (NullCrystal) variable;

            int expectedLength = 0;
            int actualLength = nullCrystal.getLength();
            assertEquals(expectedLength, actualLength, "Unexpected length for null crystal.");
        }

        // TODO: Change to ValueCrystal once it is refactored to contain the <V> value reference.
        else if (variable instanceof NumberCrystal) {
            NumberCrystal numberCrystal = (NumberCrystal) variable;
            Object actualvalue = numberCrystal.getValue();
            assertEquals(expectedValue, actualvalue, "Unexpected value.");
        } else {
            TypeCrystal typeCrystal = variable.getInstantiatedType();
            if (typeCrystal == null) {
                typeCrystal = variable.getDeclaredType();
            }
            fail("Malformed test. Unhandled type in declaration: " + typeCrystal);
        }
    }

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_VariableExpression_InVariableDeclaration() {
        String sourceString = "foo << 2, bar << foo";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_VariableExpression_InArithmeticExpression() {
        String sourceString = """
            foo << 22
            a << foo + 7
            b << foo - 7
            c << foo * 7
            d << foo / 7
            e << foo \\ 7
            """;
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 22);
        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 29);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 15);
        testVariable("c", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 154);
        testVariable("d", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 3);
        testVariable("e", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 0);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_VariableExpression_InArithmeticExpression_NumericConversions() {
        String sourceString = """
            foo << 22L
            a:Number << foo + 7
            b:Integer << foo + 7
            c:Long << foo + 7
            d:BigInteger << foo + 7
            e:Float << foo + 7
            f:Double << foo + 7
            g:BigDecimal << foo + 7
            """;
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 22L);
        testVariable("a", VikariType.NUMBER, VikariType.LONG, 29L);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 29);
        testVariable("c", VikariType.LONG, VikariType.LONG, 29L);
        testVariable("d", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("29"));
        testVariable("e", VikariType.FLOAT, VikariType.FLOAT, 29.0F);
        testVariable("f", VikariType.DOUBLE, VikariType.DOUBLE, 29.0D);
        testVariable("g", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("29", mathContext));
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_VariableExpression_InArithmeticExpression_NumericConversions_LeftAssignment() {
        String sourceString = """
            foo << 22L
            a:Number
            b:Integer
            c:Long
            d:BigInteger
            e:Float
            f:Double
            g:BigDecimal

            a << foo + 7
            b << foo + 7
            c << foo + 7
            d << foo + 7
            e << foo + 7
            f << foo + 7
            g << foo + 7
            """;
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 22L);
        testVariable("a", VikariType.NUMBER, VikariType.LONG, 29L);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 29);
        testVariable("c", VikariType.LONG, VikariType.LONG, 29L);
        testVariable("d", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("29"));
        testVariable("e", VikariType.FLOAT, VikariType.FLOAT, 29.0F);
        testVariable("f", VikariType.DOUBLE, VikariType.DOUBLE, 29.0D);
        testVariable("g", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("29", mathContext));
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_VariableExpression_InArithmeticExpression_NumericConversions_RightAssignment() {
        String sourceString = """
            foo << 22L
            a:Number
            b:Integer
            c:Long
            d:BigInteger
            e:Float
            f:Double
            g:BigDecimal

            foo + 7 >> a
            foo + 7 >> b
            foo + 7 >> c
            foo + 7 >> d
            foo + 7 >> e
            foo + 7 >> f
            foo + 7 >> g
            """;
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 22L);
        testVariable("a", VikariType.NUMBER, VikariType.LONG, 29L);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 29);
        testVariable("c", VikariType.LONG, VikariType.LONG, 29L);
        testVariable("d", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("29"));
        testVariable("e", VikariType.FLOAT, VikariType.FLOAT, 29.0F);
        testVariable("f", VikariType.DOUBLE, VikariType.DOUBLE, 29.0D);
        testVariable("g", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("29", mathContext));
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_VariableExpression_InArithmeticExpression_NumericConversions_ComplexArithmetic() {
        String sourceString = """
            a:Integer << 1
            b:Long << 2
            c:BigInteger << 3
            d:Float << 4
            e:Double << 5
            f:BigDecimal << 6

            foo:Number << a + b - c * d / e \\ f;
            """;
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("b", VikariType.LONG, VikariType.LONG, 2L);
        testVariable("c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.DOUBLE, VikariType.DOUBLE, 5.0D);
        testVariable("f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("6", mathContext));
        testVariable("foo", VikariType.NUMBER, VikariType.BIG_DECIMAL, new BigDecimal("0.5", mathContext));
    }
}
