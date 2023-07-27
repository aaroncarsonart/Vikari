package com.atonementcrystals.dnr.vikari.parser.statement;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.NullLiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Base;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_VariableDeclarationStatements extends ParserTest_Base {

    @Test
    @Order(1)
    public void testParser_Statement_VariableDeclaration_Basic() {
        String sourceString = "foo";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
    }

    @Test
    @Order(2)
    public void testParser_Statement_VariableDeclaration_WithTypeLabel() {
        String sourceString = "foo:Integer";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.INTEGER, VikariType.NULL, location(0, 0));
    }

    @Test
    @Order(3)
    public void testParser_Statement_VariableDeclaration_WithAssignment() {
        String sourceString = "foo << 2";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 2);
    }

    @Test
    @Order(4)
    public void testParser_Statement_VariableDeclaration_WithTypeLabel_AndAssignment() {
        String sourceString = "foo:Integer << 2";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2);
    }

    public void testThreeStatements(String sourceString,
                String identifier1, VikariType declaredType1, VikariType instantiatedType1, CoordinatePair location1,
                String identifier2, VikariType declaredType2, VikariType instantiatedType2, CoordinatePair location2,
                String identifier3, VikariType declaredType3, VikariType instantiatedType3, CoordinatePair location3) {

        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 3;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(parsedStatements.get(0), identifier1, declaredType1, instantiatedType1, location1);
        testDeclaration(parsedStatements.get(1), identifier2, declaredType2, instantiatedType2, location2);
        testDeclaration(parsedStatements.get(2), identifier3, declaredType3, instantiatedType3, location3);
    }

    public void testThreeStatements(String sourceString,
            String identifier1, VikariType declaredType1, VikariType instantiatedType1, CoordinatePair location1,
            Object value1,
            String identifier2, VikariType declaredType2, VikariType instantiatedType2, CoordinatePair location2,
            Object value2,
            String identifier3, VikariType declaredType3, VikariType instantiatedType3, CoordinatePair location3,
            Object value3) {

        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 3;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(parsedStatements.get(0), identifier1, declaredType1, instantiatedType1, location1, value1);
        testDeclaration(parsedStatements.get(1), identifier2, declaredType2, instantiatedType2, location2, value2);
        testDeclaration(parsedStatements.get(2), identifier3, declaredType3, instantiatedType3, location3, value3);
    }

    @Test
    @Order(5)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine() {
        testThreeStatements("foo,bar,baz",
                "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0),
                "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 4),
                "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 8));
    }

    @Test
    @Order(6)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines() {
        testThreeStatements("foo\nbar\nbaz",
                "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0),
                "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(1, 0),
                "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(2, 0));
    }

    @Test
    @Order(7)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine_WithTypeLabels() {
        testThreeStatements("foo:Integer,bar:Integer,baz:Integer",
                "foo", VikariType.INTEGER, VikariType.NULL, location(0, 0),
                "bar", VikariType.INTEGER, VikariType.NULL, location(0, 12),
                "baz", VikariType.INTEGER, VikariType.NULL, location(0, 24));
    }

    @Test
    @Order(8)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines_WithTypeLabels() {
        testThreeStatements("foo:Integer\nbar:Integer\nbaz:Integer",
                "foo", VikariType.INTEGER, VikariType.NULL, location(0, 0),
                "bar", VikariType.INTEGER, VikariType.NULL, location(1, 0),
                "baz", VikariType.INTEGER, VikariType.NULL, location(2, 0));
    }

    @Test
    @Order(9)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine_WithInitializerExpressions() {
        testThreeStatements("foo << 2, bar << 3, baz << 4",
                "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 10), 3,
                "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 20), 4);
    }

    @Test
    @Order(10)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines_WithInitializerExpressions() {
        testThreeStatements("foo << 2\nbar << 3\nbaz << 4",
                "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 3,
                "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(2, 0), 4);
    }

    @Test
    @Order(11)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine_WithTypeLabels_AndInitializerExpressions() {
        testThreeStatements("foo:Integer << 2, bar:Integer << 3, baz:Integer << 4",
                "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.INTEGER, VikariType.INTEGER, location(0, 18), 3,
                "baz", VikariType.INTEGER, VikariType.INTEGER, location(0, 36), 4);
    }

    @Test
    @Order(12)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines_WithTypeLabels_AndInitializerExpressions() {
        testThreeStatements("foo:Integer << 2\nbar:Integer << 3\nbaz:Integer << 4",
                "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.INTEGER, VikariType.INTEGER, location(1, 0), 3,
                "baz", VikariType.INTEGER, VikariType.INTEGER, location(2, 0), 4);
    }

    @Test
    @Order(13)
    public void testParser_Statement_VariableDeclaration_AllNumericTypes() {
        String sourceString = """
                a:Integer
                b:Long
                c:BigInteger
                d:Float
                e:Double
                f:BigDecimal
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 6;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.LONG, VikariType.NULL, location(1, 0));
        testDeclaration(statements.get(2), "c", VikariType.BIG_INTEGER, VikariType.NULL, location(2, 0));
        testDeclaration(statements.get(3), "d", VikariType.FLOAT, VikariType.NULL, location(3, 0));
        testDeclaration(statements.get(4), "e", VikariType.DOUBLE, VikariType.NULL, location(4, 0));
        testDeclaration(statements.get(5), "f", VikariType.BIG_DECIMAL, VikariType.NULL, location(5, 0));
    }

    @Test
    @Order(14)
    public void testParser_Statement_VariableDeclaration_AllNumericTypes_WithInitializer() {
        String sourceString = """
                a:Integer << 1
                b:Long << 2L
                c:BigInteger << 3B
                d:Float << 4.0F
                e:Double << 5.0D
                f:BigDecimal << 6.0B
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 6;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        BigInteger bigInteger = new BigInteger("3");
        BigDecimal bigDecimal = new BigDecimal("6.0", Arithmetic.getMathContext());

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration(statements.get(1), "b", VikariType.LONG, VikariType.LONG, location(1, 0), 2L);
        testDeclaration(statements.get(2), "c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, location(2, 0),
                bigInteger);
        testDeclaration(statements.get(3), "d", VikariType.FLOAT, VikariType.FLOAT, location(3, 0), 4.0F);
        testDeclaration(statements.get(4), "e", VikariType.DOUBLE, VikariType.DOUBLE, location(4, 0), 5.0D);
        testDeclaration(statements.get(5), "f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, location(5, 0),
                bigDecimal);
    }

    @Test
    @Order(15)
    public void testParser_Statement_VariableDeclaration_NumericUpcasts() {
        String sourceString = """
                a:Long << 1
                b:BigInteger << 2
                c:BigInteger << 3L
                d:Float << 4
                e:Float << 5L
                f:Float << 6B
                g:Double << 7
                h:Double << 8L
                i:Double << 9B
                j:Double << 10.0F
                k:BigDecimal << 11
                l:BigDecimal << 12L
                m:BigDecimal << 13B
                n:BigDecimal << 14F
                o:BigDecimal << 15D
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 15;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.LONG, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration(statements.get(1), "b", VikariType.BIG_INTEGER, VikariType.INTEGER, location(1, 0), 2);
        testDeclaration(statements.get(2), "c", VikariType.BIG_INTEGER, VikariType.LONG, location(2, 0), 3L);
        testDeclaration(statements.get(3), "d", VikariType.FLOAT, VikariType.INTEGER, location(3, 0), 4);
        testDeclaration(statements.get(4), "e", VikariType.FLOAT, VikariType.LONG, location(4, 0), 5L);
        testDeclaration(statements.get(5), "f", VikariType.FLOAT, VikariType.BIG_INTEGER, location(5, 0), new BigInteger("6"));
        testDeclaration(statements.get(6), "g", VikariType.DOUBLE, VikariType.INTEGER, location(6, 0), 7);
        testDeclaration(statements.get(7), "h", VikariType.DOUBLE, VikariType.LONG, location(7, 0), 8L);
        testDeclaration(statements.get(8), "i", VikariType.DOUBLE, VikariType.BIG_INTEGER, location(8, 0), new BigInteger("9"));
        testDeclaration(statements.get(9), "j", VikariType.DOUBLE, VikariType.FLOAT, location(9, 0), 10.0F);
        testDeclaration(statements.get(10), "k", VikariType.BIG_DECIMAL, VikariType.INTEGER, location(10, 0), 11);
        testDeclaration(statements.get(11), "l", VikariType.BIG_DECIMAL, VikariType.LONG, location(11, 0), 12L);
        testDeclaration(statements.get(12), "m", VikariType.BIG_DECIMAL, VikariType.BIG_INTEGER, location(12, 0), new BigInteger("13"));
        testDeclaration(statements.get(13), "n", VikariType.BIG_DECIMAL, VikariType.FLOAT, location(13, 0), 14.0F);
        testDeclaration(statements.get(14), "o", VikariType.BIG_DECIMAL, VikariType.DOUBLE, location(14, 0), 15.0D);
    }

    @Test
    @Order(16)
    public void testParser_Statement_VariableDeclaration_NumericDowncasts() {
        String sourceString = """
                a:Integer << 1L
                b:Integer << 2B
                c:Integer << 3.0F
                d:Integer << 4.0D
                e:Integer << 5.0B
                f:Long << 6B
                g:Long << 7.0F
                h:Long << 8.0D
                i:Long << 9.0B
                j:BigInteger << 10.0F
                k:BigInteger << 11.0D
                l:BigInteger << 12.0B
                m:Float << 13.0D
                n:Float << 14.0B
                o:Double << 15.0B
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 15;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        MathContext mathContext = Arithmetic.getMathContext();

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, VikariType.LONG, location(0, 0), 1L);
        testDeclaration(statements.get(1), "b", VikariType.INTEGER, VikariType.BIG_INTEGER, location(1, 0), new BigInteger("2"));
        testDeclaration(statements.get(2), "c", VikariType.INTEGER, VikariType.FLOAT, location(2, 0), 3.0F);
        testDeclaration(statements.get(3), "d", VikariType.INTEGER, VikariType.DOUBLE, location(3, 0), 4.0D);
        testDeclaration(statements.get(4), "e", VikariType.INTEGER, VikariType.BIG_DECIMAL, location(4, 0), new BigDecimal("5.0", mathContext));
        testDeclaration(statements.get(5), "f", VikariType.LONG, VikariType.BIG_INTEGER, location(5, 0), new BigInteger("6"));
        testDeclaration(statements.get(6), "g", VikariType.LONG, VikariType.FLOAT, location(6, 0), 7.0F);
        testDeclaration(statements.get(7), "h", VikariType.LONG, VikariType.DOUBLE, location(7, 0), 8.0D);
        testDeclaration(statements.get(8), "i", VikariType.LONG, VikariType.BIG_DECIMAL, location(8, 0), new BigDecimal("9.0", mathContext));
        testDeclaration(statements.get(9), "j", VikariType.BIG_INTEGER, VikariType.FLOAT, location(9, 0), 10.0F);
        testDeclaration(statements.get(10), "k", VikariType.BIG_INTEGER, VikariType.DOUBLE, location(10, 0), 11.0D);
        testDeclaration(statements.get(11), "l", VikariType.BIG_INTEGER, VikariType.BIG_DECIMAL, location(11, 0), new BigDecimal("12.0", mathContext));
        testDeclaration(statements.get(12), "m", VikariType.FLOAT, VikariType.DOUBLE, location(12, 0), 13.0D);
        testDeclaration(statements.get(13), "n", VikariType.FLOAT, VikariType.BIG_DECIMAL, location(13, 0), new BigDecimal("14.0", mathContext));
        testDeclaration(statements.get(14), "o", VikariType.DOUBLE, VikariType.BIG_DECIMAL, location(14, 0), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(17)
    public void testParser_Statement_VariableDeclaration_AssignmentToParentTypes() {
        String sourceString = """
                a << 1
                b:AtonementCrystal << 2
                c:Value << 3
                d:Number << 4
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 4;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 2);
        testDeclaration(statements.get(2), "c", VikariType.VALUE, VikariType.INTEGER, location(2, 0), 3);
        testDeclaration(statements.get(3), "d", VikariType.NUMBER, VikariType.INTEGER, location(3, 0), 4);
    }

    @Test
    @Order(18)
    public void testParser_Statement_VariableDeclaration_ErrorCase_DuplicateDeclaration_SameType() {
        String sourceString = """
                a:Integer << 1
                a:Integer << 2
                """;

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(1, 0), "a:Integer << 2", "Variable is already defined.");

        int expectedSize = 2;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration_DuplicateError(statements.get(1), "a", VikariType.INTEGER, VikariType.INTEGER, location(1, 0), 2);
    }

    @Test
    @Order(19)
    public void testParser_Statement_VariableDeclaration_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo", "Unknown Type.");
        testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar", "Unknown Type.");

        int expectedSize = 2;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(1, 0));
    }

    @Test
    @Order(20)
    public void testParser_Statement_VariableDeclaration_ErrorCase_UnknownType_WithAssignment() {
        String sourceString = """
                foo:Foo << 22
                bar:Bar << 7L
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo << 22", "Unknown Type.");
        testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar << 7L", "Unknown Type.");

        int expectedSize = 2;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 22);
        testDeclaration(statements.get(1), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, location(1, 0), 7L);
    }

    @Test
    @Order(21)
    public void testParser_Statement_VariableDeclaration_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type << 2";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 0), "foo:Type << 2", "Variable with " +
                "type Type cannot be assigned a value of type Integer.");

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration_TypeError(statements.get(0), "foo", VikariType.TYPE, VikariType.INTEGER, location(0, 0), 2);
    }

    @Test
    @Order(22)
    public void testParser_Statement_VariableDeclaration_BooleanValues() {
        String sourceString = """
                ~:1. no declared type.:~
                a << true
                b << false

                ~:2. No initializer expression.:~
                c:Boolean

                ~:3. With initializer expression.:~
                d:Boolean << true
                e:Boolean << false

                ~:4. Assignment to parent types.:~
                f:AtonementCrystal << true
                g:Value << false
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 7;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, location(1, 0), true);
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, location(2, 0), false);
        testDeclaration(statements.get(2), "c", VikariType.BOOLEAN, VikariType.NULL, location(5, 0));
        testDeclaration(statements.get(3), "d", VikariType.BOOLEAN, VikariType.BOOLEAN, location(8, 0), true);
        testDeclaration(statements.get(4), "e", VikariType.BOOLEAN, VikariType.BOOLEAN, location(9, 0), false);
        testDeclaration(statements.get(5), "f", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, location(12, 0), true);
        testDeclaration(statements.get(6), "g", VikariType.VALUE, VikariType.BOOLEAN, location(13, 0), false);
    }

    @Test
    @Order(23)
    public void testParser_Statement_VariableDeclaration_SyntaxError_InvalidTypeAssignment_BooleanValues() {
        String sourceString = """
                a:Boolean << 2
                b:Integer << true
                c:Type << false
                """;

        int expectedErrorCount = 3;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(0, 0), "a:Boolean << 2", "Variable with type Boolean cannot be assigned a value of type Integer.");
        testSyntaxError(syntaxErrors.get(1), location(1, 0), "b:Integer << true", "Variable with type Integer cannot be assigned a value of type Boolean.");
        testSyntaxError(syntaxErrors.get(2), location(2, 0), "c:Type << false", "Variable with type Type cannot be assigned a value of type Boolean.");

        int expectedSize = 3;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration_TypeError(statements.get(0), "a", VikariType.BOOLEAN, VikariType.INTEGER, location(0, 0), 2);
        testDeclaration_TypeError(statements.get(1), "b", VikariType.INTEGER, VikariType.BOOLEAN, location(1, 0), true);
        testDeclaration_TypeError(statements.get(2), "c", VikariType.TYPE, VikariType.BOOLEAN, location(2, 0), false);
    }

    @Test
    @Order(24)
    public void testParser_Statement_VariableDeclaration_SyntaxError_InvalidTypeAssignment_BooleanValues_FromVariable() {
        String sourceString = """
                bool_1:Boolean << true
                int_1:Integer << 2

                bool_2:Boolean << int_1
                int_2:Integer << bool_1
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        testSyntaxError(syntaxErrors.get(0), location(3, 0), "bool_2:Boolean << int_1", "Variable with type Boolean cannot be assigned a value of type Integer.");
        testSyntaxError(syntaxErrors.get(1), location(4, 0), "int_2:Integer << bool_1", "Variable with type Integer cannot be assigned a value of type Boolean.");

        int expectedSize = 4;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "bool_1", VikariType.BOOLEAN, VikariType.BOOLEAN, location(0, 0), true);
        testDeclaration(statements.get(1), "int_1", VikariType.INTEGER, VikariType.INTEGER, location(1, 0), 2);

        testDeclaration_FromVariable_TypeError(statements.get(2), "bool_2", VikariType.BOOLEAN, location(3, 0), "int_1",
                VikariType.INTEGER, VikariType.INTEGER, location(3, 18));

        testDeclaration_FromVariable_TypeError(statements.get(3), "int_2", VikariType.INTEGER, location(4, 0), "bool_1",
                VikariType.BOOLEAN, VikariType.BOOLEAN, location(4, 17));
    }

    @Test
    @Order(25)
    public void testParser_Statement_VariableDeclaration_Nulls_LengthZero() {
        String sourceString = """
                a
                b << null
                c << _[0]_
                d:Integer
                e:Integer << null
                f:Integer << __[0]__
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration_NullKeyword(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, location(1, 0), location(1, 5));
        testDeclaration_NullLiteralExpression(statements.get(2), "c", VikariType.ATONEMENT_CRYSTAL, location(2, 0),
                location(2, 5), location(2, 7), 0);

        testDeclaration(statements.get(3), "d", VikariType.INTEGER, VikariType.NULL, location(3, 0));
        testDeclaration_NullKeyword(statements.get(4), "e", VikariType.INTEGER, location(4, 0), location(4, 13));
        testDeclaration_NullLiteralExpression(statements.get(5), "f", VikariType.INTEGER, location(5, 0),
                location(5, 13), location(5, 16), 0);
    }

    @Test
    @Order(26)
    public void testParser_Statement_VariableDeclaration_Nulls_PositiveLength() {
        String sourceString = """
                a << _
                b << __
                c << ___
                d << ____
                e << _____
                f << _[1]_
                g << _[2]_
                h << _[3]_
                i << _[4]_
                j << _[5]_
                k:Integer << _
                l:Integer << __
                m:Integer << ___
                n:Integer << ____
                o:Integer << _____
                p:Integer << _[1]_
                q:Integer << _[2]_
                r:Integer << _[3]_
                s:Integer << _[4]_
                t:Integer << _[5]_
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration_NullSwordLiteral(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, location(0, 0),
                location(0, 5), 1);
        testDeclaration_NullSwordLiteral(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, location(1, 0),
                location(1, 5), 2);
        testDeclaration_NullSwordLiteral(statements.get(2), "c", VikariType.ATONEMENT_CRYSTAL, location(2, 0),
                location(2, 5), 3);
        testDeclaration_NullSwordLiteral(statements.get(3), "d", VikariType.ATONEMENT_CRYSTAL, location(3, 0),
                location(3, 5), 4);
        testDeclaration_NullSwordLiteral(statements.get(4), "e", VikariType.ATONEMENT_CRYSTAL, location(4, 0),
                location(4, 5), 5);

        testDeclaration_NullLiteralExpression(statements.get(5), "f", VikariType.ATONEMENT_CRYSTAL, location(5, 0),
                location(5, 5), location(5, 7), 1);
        testDeclaration_NullLiteralExpression(statements.get(6), "g", VikariType.ATONEMENT_CRYSTAL, location(6, 0),
                location(6, 5), location(6, 7), 2);
        testDeclaration_NullLiteralExpression(statements.get(7), "h", VikariType.ATONEMENT_CRYSTAL, location(7, 0),
                location(7, 5), location(7, 7), 3);
        testDeclaration_NullLiteralExpression(statements.get(8), "i", VikariType.ATONEMENT_CRYSTAL, location(8, 0),
                location(8, 5), location(8, 7), 4);
        testDeclaration_NullLiteralExpression(statements.get(9), "j", VikariType.ATONEMENT_CRYSTAL, location(9, 0),
                location(9, 5), location(9, 7), 5);

        testDeclaration_NullSwordLiteral(statements.get(10), "k", VikariType.INTEGER, location(10, 0),
                location(10, 13), 1);
        testDeclaration_NullSwordLiteral(statements.get(11), "l", VikariType.INTEGER, location(11, 0),
                location(11, 13), 2);
        testDeclaration_NullSwordLiteral(statements.get(12), "m", VikariType.INTEGER, location(12, 0),
                location(12, 13), 3);
        testDeclaration_NullSwordLiteral(statements.get(13), "n", VikariType.INTEGER, location(13, 0),
                location(13, 13), 4);
        testDeclaration_NullSwordLiteral(statements.get(14), "o", VikariType.INTEGER, location(14, 0),
                location(14, 13), 5);

        testDeclaration_NullLiteralExpression(statements.get(15), "p", VikariType.INTEGER, location(15, 0),
                location(15, 13), location(15, 15), 1);
        testDeclaration_NullLiteralExpression(statements.get(16), "q", VikariType.INTEGER, location(16, 0),
                location(16, 13), location(16, 15), 2);
        testDeclaration_NullLiteralExpression(statements.get(17), "r", VikariType.INTEGER, location(17, 0),
                location(17, 13), location(17, 15), 3);
        testDeclaration_NullLiteralExpression(statements.get(18), "s", VikariType.INTEGER, location(18, 0),
                location(18, 13), location(18, 15), 4);
        testDeclaration_NullLiteralExpression(statements.get(19), "t", VikariType.INTEGER, location(19, 0),
                location(19, 13), location(19, 15), 5);
    }

    @Test
    @Order(27)
    public void testParser_Statement_VariableDeclaration_Nulls_NegativeLength() {
        String sourceString = """
                a << _[-1]_
                b << _[-2]_
                c << _[-3]_
                d << _[-4]_
                e << _[-5]_
                f:Integer << _[-1]_
                g:Integer << _[-2]_
                h:Integer << _[-3]_
                i:Integer << _[-4]_
                j:Integer << _[-5]_
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration_NullLiteralExpression(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, location(0, 0),
                location(0, 5), location(0, 8), -1);
        testDeclaration_NullLiteralExpression(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, location(1, 0),
                location(1, 5), location(1, 8), -2);
        testDeclaration_NullLiteralExpression(statements.get(2), "c", VikariType.ATONEMENT_CRYSTAL, location(2, 0),
                location(2, 5), location(2, 8), -3);
        testDeclaration_NullLiteralExpression(statements.get(3), "d", VikariType.ATONEMENT_CRYSTAL, location(3, 0),
                location(3, 5), location(3, 8), -4);
        testDeclaration_NullLiteralExpression(statements.get(4), "e", VikariType.ATONEMENT_CRYSTAL, location(4, 0),
                location(4, 5), location(4, 8), -5);

        testDeclaration_NullLiteralExpression(statements.get(5), "f", VikariType.INTEGER, location(5, 0),
                location(5, 13), location(5, 16), -1);
        testDeclaration_NullLiteralExpression(statements.get(6), "g", VikariType.INTEGER, location(6, 0),
                location(6, 13), location(6, 16), -2);
        testDeclaration_NullLiteralExpression(statements.get(7), "h", VikariType.INTEGER, location(7, 0),
                location(7, 13), location(7, 16), -3);
        testDeclaration_NullLiteralExpression(statements.get(8), "i", VikariType.INTEGER, location(8, 0),
                location(8, 13), location(8, 16), -4);
        testDeclaration_NullLiteralExpression(statements.get(9), "j", VikariType.INTEGER, location(9, 0),
                location(9, 13), location(9, 16), -5);
    }

    @Test
    @Order(28)
    public void testParser_Statement_VariableDeclaration_Nulls_NullLiteralExpression_WithVariable() {
        String sourceString = """
                int:Integer << 2
                foo << __[int]__
                """;
        List<Statement> statements = lexAndParse(sourceString);
        testDeclaration(statements.get(0), "int", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2);

        // null literal expression with variable
        Statement statement = statements.get(1);
        assertEquals(location(1, 0), statement.getLocation(), "Unexpected location.");

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(1, 0));

        BinaryOperatorCrystal expectedOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, expectedOperator.getClass(), "Unexpected operator type.");

        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(location(1, 7), initializerExpression.getLocation(), "Unexpected location.");
        assertEquals(NullLiteralExpression.class, initializerExpression.getClass(), "Unexpected expression type.");

        NullLiteralExpression nullLiteralExpression = (NullLiteralExpression) initializerExpression;
        Expression innerExpression = nullLiteralExpression.getExpression();
        assertEquals(VariableExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        VariableExpression variableExpression = (VariableExpression) innerExpression;
        AtonementCrystal reference = variableExpression.getReference();
        testVariableCrystal(reference, "int", VikariType.INTEGER, VikariType.INTEGER, location(1, 10));
    }

    @Test
    @Order(29)
    public void testParser_Statement_VariableDeclaration_Nulls_NullLiteralExpression_WithArithmeticExpression() {
        String sourceString = """
                foo << __[5 + 7]__
                """;
        List<Statement> statements = lexAndParse(sourceString);

        // null literal expression with arithmetic expression
        Statement statement = statements.get(0);
        assertEquals(location(0, 0), statement.getLocation(), "Unexpected location.");

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));

        BinaryOperatorCrystal expectedOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, expectedOperator.getClass(), "Unexpected operator type.");

        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(location(0, 7), initializerExpression.getLocation(), "Unexpected location.");
        assertEquals(NullLiteralExpression.class, initializerExpression.getClass(), "Unexpected expression type.");

        NullLiteralExpression nullLiteralExpression = (NullLiteralExpression) initializerExpression;
        Expression innerExpression = nullLiteralExpression.getExpression();
        assertEquals(location(0, 10), innerExpression.getLocation(), "Unexpected location.");
        assertEquals(BinaryExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        BinaryExpression binaryExpression = (BinaryExpression) innerExpression;
        Expression leftExpression = binaryExpression.getLeft();
        BinaryOperatorCrystal binaryOperator = binaryExpression.getOperator();
        Expression rightExpression = binaryExpression.getRight();

        assertEquals(LiteralExpression.class, leftExpression.getClass(), "Unexpected expression type.");
        assertEquals(AddOperatorCrystal.class, binaryOperator.getClass(), "Unexpected operator type.");
        assertEquals(LiteralExpression.class, rightExpression.getClass(), "Unexpected expression type.");

        AtonementCrystal leftOperand = ((LiteralExpression) leftExpression).getValue();
        AtonementCrystal rightOperand = ((LiteralExpression) rightExpression).getValue();

        assertEquals(location(0, 10), leftOperand.getCoordinates(), "Unexpected location.");
        assertEquals(location(0, 12), binaryOperator.getCoordinates(), "Unexpected location.");
        assertEquals(location(0, 14), rightOperand.getCoordinates(), "Unexpected location.");

        testNumberCrystal(leftOperand, 5, IntegerCrystal.class);
        testNumberCrystal(rightOperand, 7, IntegerCrystal.class);
    }
}
