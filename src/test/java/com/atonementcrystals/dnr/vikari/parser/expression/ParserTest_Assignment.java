package com.atonementcrystals.dnr.vikari.parser.expression;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.RightAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.NullLiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Base;
import com.atonementcrystals.dnr.vikari.parser.ParserTest_Utils;
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

/**
 * Leading variable declaration statements are necessary in order to test assignment expressions.
 * However, these declarations will not be explicitly tested in this test class, except for
 * asserting they have the correct statement type.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_Assignment extends ParserTest_Base {

    // -------------------------------
    // Left assignment operator tests.
    // -------------------------------

    @Test
    @Order(1)
    public void testParser_Expression_LeftAssignment_Basic() {
        List<Statement> statements = lexAndParse("foo,foo << 2");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleLeftAssignment(statement, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 4), 2);
    }

    @Test
    @Order(2)
    public void testParser_Expression_LeftAssignment_WithTypeLabel() {
        String sourceString = "foo:Integer, foo << 3";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleLeftAssignment(statement, "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 13), 3);
    }

    @Test
    @Order(3)
    public void testParser_Expression_LeftAssignment_WithTypeLabel_ReAssignment() {
        String sourceString = "foo:Integer << 2, foo << 4";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleLeftAssignment(statement, "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 18), 4);
    }

    @Test
    @Order(4)
    public void testParser_Expression_LeftAssignment_MultipleVariables_SingleLine() {
        String sourceString = "foo, bar, baz, foo << 1, bar << 2, baz << 3";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 15), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 25), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 35), 3);
    }

    @Test
    @Order(5)
    public void testParser_Expression_LeftAssignment_MultipleVariables_MultipleLines() {
        String sourceString = """
                foo
                bar
                baz
                foo << 1
                bar << 2
                baz << 3
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(3, 0), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(4, 0), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(5, 0), 3);
    }

    @Test
    @Order(6)
    public void testParser_Expression_LeftAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
        String sourceString = "foo:Integer, bar:Integer, baz:Integer, foo << 1, bar << 2, baz << 3";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 39), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, location(0, 49), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, location(0, 59), 3);
    }

    @Test
    @Order(7)
    public void testParser_Expression_LeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
        String sourceString = """
                foo:Integer
                bar:Integer
                baz:Integer
                foo << 1
                bar << 2
                baz << 3
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 0), 1);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 0), 2);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 0), 3);
    }

    @Test
    @Order(8)
    public void testParser_Expression_LeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = """
                foo:Integer << 1
                bar:Integer << 2
                baz:Integer << 3
                foo << 4
                bar << 5
                baz << 6
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(3), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 0), 4);
        testSimpleLeftAssignment(statements.get(4), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 0), 5);
        testSimpleLeftAssignment(statements.get(5), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 0), 6);
    }

    @Test
    @Order(9)
    public void testParser_Expression_LeftAssignment_AllNumericTypes() {
        String sourceString = """
                a, b, c, d, e, f
                a << 1
                b << 2L
                c << 3B
                d << 4.0F
                e << 5.0D
                f << 6.0B
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(6), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 1);
        testSimpleLeftAssignment(statements.get(7), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, location(2, 0), 2L);
        testSimpleLeftAssignment(statements.get(8), "c", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_INTEGER, location(3, 0), new BigInteger("3"));
        testSimpleLeftAssignment(statements.get(9), "d", VikariType.ATONEMENT_CRYSTAL, VikariType.FLOAT, location(4, 0), 4.0F);
        testSimpleLeftAssignment(statements.get(10), "e", VikariType.ATONEMENT_CRYSTAL, VikariType.DOUBLE, location(5, 0), 5.0D);
        testSimpleLeftAssignment(statements.get(11), "f", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_DECIMAL, location(6, 0), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(10)
    public void testParser_Expression_LeftAssignment_AllNumericTypes_WithTypeLabels() {
        String sourceString = """
                a:Integer
                b:Long
                c:BigInteger
                d:Float
                e:Double
                f:BigDecimal
                a << 1
                b << 2L
                c << 3B
                d << 4.0F
                e << 5.0D
                f << 6.0B
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(6), "a", VikariType.INTEGER, VikariType.INTEGER, location(6, 0), 1);
        testSimpleLeftAssignment(statements.get(7), "b", VikariType.LONG, VikariType.LONG, location(7, 0), 2L);
        testSimpleLeftAssignment(statements.get(8), "c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, location(8, 0), new BigInteger("3"));
        testSimpleLeftAssignment(statements.get(9), "d", VikariType.FLOAT, VikariType.FLOAT, location(9, 0), 4.0F);
        testSimpleLeftAssignment(statements.get(10), "e", VikariType.DOUBLE, VikariType.DOUBLE, location(10, 0), 5.0D);
        testSimpleLeftAssignment(statements.get(11), "f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, location(11, 0), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(11)
    public void testParser_Expression_LeftAssignment_NumericUpcasts() {
        String sourceString = """
                long1:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                float1:Float
                float2:Float
                float3:Float
                double1:Double
                double2:Double
                double3:Double
                double4:Double
                bigDecimal1:BigDecimal
                bigDecimal2:BigDecimal
                bigDecimal3:BigDecimal
                bigDecimal4:BigDecimal
                bigDecimal5:BigDecimal

                long1 << 1
                bigInteger1 << 2
                bigInteger2 << 3L
                float1 << 4
                float2 << 5L
                float3 << 6B
                double1 << 7
                double2 << 8L
                double3 << 9B
                double4 << 10.0F
                bigDecimal1 << 11
                bigDecimal2 << 12L
                bigDecimal3 << 13B
                bigDecimal4 << 14.0F
                bigDecimal5 << 15.0D
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }

        // assignment statements
        testSimpleLeftAssignment(statements.get(15), "long1", VikariType.LONG, VikariType.INTEGER, location(16, 0), 1);
        testSimpleLeftAssignment(statements.get(16), "bigInteger1", VikariType.BIG_INTEGER, VikariType.INTEGER, location(17, 0), 2);
        testSimpleLeftAssignment(statements.get(17), "bigInteger2", VikariType.BIG_INTEGER, VikariType.LONG, location(18, 0), 3L);
        testSimpleLeftAssignment(statements.get(18), "float1", VikariType.FLOAT, VikariType.INTEGER, location(19, 0), 4);
        testSimpleLeftAssignment(statements.get(19), "float2", VikariType.FLOAT, VikariType.LONG, location(20, 0), 5L);
        testSimpleLeftAssignment(statements.get(20), "float3", VikariType.FLOAT, VikariType.BIG_INTEGER, location(21, 0), new BigInteger("6"));
        testSimpleLeftAssignment(statements.get(21), "double1", VikariType.DOUBLE, VikariType.INTEGER, location(22, 0), 7);
        testSimpleLeftAssignment(statements.get(22), "double2", VikariType.DOUBLE, VikariType.LONG, location(23, 0), 8L);
        testSimpleLeftAssignment(statements.get(23), "double3", VikariType.DOUBLE, VikariType.BIG_INTEGER, location(24, 0), new BigInteger("9"));
        testSimpleLeftAssignment(statements.get(24), "double4", VikariType.DOUBLE, VikariType.FLOAT, location(25, 0), 10.0F);
        testSimpleLeftAssignment(statements.get(25), "bigDecimal1", VikariType.BIG_DECIMAL, VikariType.INTEGER, location(26, 0), 11);
        testSimpleLeftAssignment(statements.get(26), "bigDecimal2", VikariType.BIG_DECIMAL, VikariType.LONG, location(27, 0), 12L);
        testSimpleLeftAssignment(statements.get(27), "bigDecimal3", VikariType.BIG_DECIMAL, VikariType.BIG_INTEGER, location(28, 0), new BigInteger("13"));
        testSimpleLeftAssignment(statements.get(28), "bigDecimal4", VikariType.BIG_DECIMAL, VikariType.FLOAT, location(29, 0), 14.0F);
        testSimpleLeftAssignment(statements.get(29), "bigDecimal5", VikariType.BIG_DECIMAL, VikariType.DOUBLE, location(30, 0), 15.0D);
    }

    @Test
    @Order(12)
    public void testParser_Expression_LeftAssignment_NumericDowncasts() {
        String sourceString = """
                integer1:Integer
                integer2:Integer
                integer3:Integer
                integer4:Integer
                integer5:Integer
                long1:Long
                long2:Long
                long3:Long
                long4:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                bigInteger3:BigInteger
                float1:Float
                float2:Float
                double1:Double

                integer1 << 1L
                integer2 << 2B
                integer3 << 3.0F
                integer4 << 4.0D
                integer5 << 5.0B
                long1 << 6B
                long2 << 7.0F
                long3 << 8.0D
                long4 << 9.0B
                bigInteger1 << 10.0F
                bigInteger2 << 11.0D
                bigInteger3 << 12.0B
                float1 << 13.0D
                float2 << 14.0B
                double1 << 15.0B
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }

        MathContext mathContext = Arithmetic.getMathContext();

        // assignment statements
        testSimpleLeftAssignment(statements.get(15), "integer1", VikariType.INTEGER, VikariType.LONG, location(16, 0), 1L);
        testSimpleLeftAssignment(statements.get(16), "integer2", VikariType.INTEGER, VikariType.BIG_INTEGER, location(17, 0), new BigInteger("2"));
        testSimpleLeftAssignment(statements.get(17), "integer3", VikariType.INTEGER, VikariType.FLOAT, location(18, 0), 3.0F);
        testSimpleLeftAssignment(statements.get(18), "integer4", VikariType.INTEGER, VikariType.DOUBLE, location(19, 0), 4.0D);
        testSimpleLeftAssignment(statements.get(19), "integer5", VikariType.INTEGER, VikariType.BIG_DECIMAL, location(20, 0), new BigDecimal("5.0", mathContext));
        testSimpleLeftAssignment(statements.get(20), "long1", VikariType.LONG, VikariType.BIG_INTEGER, location(21, 0), new BigInteger("6"));
        testSimpleLeftAssignment(statements.get(21), "long2", VikariType.LONG, VikariType.FLOAT, location(22, 0), 7.0F);
        testSimpleLeftAssignment(statements.get(22), "long3", VikariType.LONG, VikariType.DOUBLE, location(23, 0), 8.0D);
        testSimpleLeftAssignment(statements.get(23), "long4", VikariType.LONG, VikariType.BIG_DECIMAL, location(24, 0), new BigDecimal("9.0", mathContext));
        testSimpleLeftAssignment(statements.get(24), "bigInteger1", VikariType.BIG_INTEGER, VikariType.FLOAT, location(25, 0), 10.0F);
        testSimpleLeftAssignment(statements.get(25), "bigInteger2", VikariType.BIG_INTEGER, VikariType.DOUBLE, location(26, 0), 11.0D);
        testSimpleLeftAssignment(statements.get(26), "bigInteger3", VikariType.BIG_INTEGER, VikariType.BIG_DECIMAL, location(27, 0), new BigDecimal("12.0", mathContext));
        testSimpleLeftAssignment(statements.get(27), "float1", VikariType.FLOAT, VikariType.DOUBLE, location(28, 0), 13.0D);
        testSimpleLeftAssignment(statements.get(28), "float2", VikariType.FLOAT, VikariType.BIG_DECIMAL, location(29, 0), new BigDecimal("14.0", mathContext));
        testSimpleLeftAssignment(statements.get(29), "double1", VikariType.DOUBLE, VikariType.BIG_DECIMAL, location(30, 0), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(13)
    public void testParser_Expression_LeftAssignment_AssignmentToParentTypes() {
        String sourceString = """
                a, b:AtonementCrystal, c:Value, d:Number
                a << 1
                b << 2
                c << 3
                d << 4
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 8;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(4), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 1);
        testSimpleLeftAssignment(statements.get(5), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(2, 0), 2);
        testSimpleLeftAssignment(statements.get(6), "c", VikariType.VALUE, VikariType.INTEGER, location(3, 0), 3);
        testSimpleLeftAssignment(statements.get(7), "d", VikariType.NUMBER, VikariType.INTEGER, location(4, 0), 4);
    }

    @Test
    @Order(14)
    public void testParser_Expression_LeftAssignment_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                foo << 2
                bar << 4
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar", "Unknown Type.");

        int expectedStatementCount = 4;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment(statements.get(2), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(2, 0), 2);
        testSimpleLeftAssignment(statements.get(3), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(3, 0), 4);
    }

    @Test
    @Order(15)
    public void testParser_Expression_LeftAssignment_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type, foo << 2";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 10), "foo:Type, foo << 2", "Variable " +
                "with type Type cannot be assigned a value of type Integer.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleLeftAssignment_TypeError(statements.get(1), "foo", VikariType.TYPE, VikariType.INTEGER, location(0, 10), 2);
    }

    // --------------------------------
    // Right assignment operator tests.
    // --------------------------------

    @Test
    @Order(16)
    public void testParser_Expression_RightAssignment_Basic() {
        List<Statement> statements = lexAndParse("foo,2 >> foo");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleRightAssignment(statement, location(0, 4), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 9), 2);
    }

    @Test
    @Order(17)
    public void testParser_Expression_RightAssignment_WithTypeLabel() {
        String sourceString = "foo:Integer, 3 >> foo";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleRightAssignment(statement, location(0, 13), "foo", VikariType.INTEGER, VikariType.INTEGER,
                location(0, 18), 3);
    }

    @Test
    @Order(18)
    public void testParser_Expression_RightAssignment_WithTypeLabel_ReAssignment() {
        String sourceString = "foo:Integer << 2, 4 >> foo";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // statement 1
        Statement statement = statements.get(0);
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");

        // statement 2
        statement = statements.get(1);
        testSimpleRightAssignment(statement, location(0, 18), "foo", VikariType.INTEGER, VikariType.INTEGER,
                location(0, 23), 4);
    }

    @Test
    @Order(19)
    public void testParser_Expression_RightAssignment_MultipleVariables_SingleLine() {
        String sourceString = "foo, bar, baz, 1 >> foo, 2 >> bar, 3 >> baz";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), location(0, 15), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(0, 20), 1);

        testSimpleRightAssignment(statements.get(4), location(0, 25), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(0, 30), 2);

        testSimpleRightAssignment(statements.get(5), location(0, 35), "baz", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(0, 40), 3);
    }

    @Test
    @Order(20)
    public void testParser_Expression_RightAssignment_MultipleVariables_MultipleLines() {
        String sourceString = """
                foo
                bar
                baz
                1 >> foo
                2 >> bar
                3 >> baz
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), location(3, 0), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(3, 5), 1);

        testSimpleRightAssignment(statements.get(4), location(4, 0), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(4, 5), 2);

        testSimpleRightAssignment(statements.get(5), location(5, 0), "baz", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(5, 5), 3);
    }

    @Test
    @Order(21)
    public void testParser_Expression_RightAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
        String sourceString = "foo:Integer, bar:Integer, baz:Integer, 1 >> foo, 2 >> bar, 3 >> baz";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), location(0, 39), "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 44), 1);
        testSimpleRightAssignment(statements.get(4), location(0, 49) ,"bar", VikariType.INTEGER, VikariType.INTEGER, location(0, 54), 2);
        testSimpleRightAssignment(statements.get(5), location(0, 59), "baz", VikariType.INTEGER, VikariType.INTEGER, location(0, 64), 3);
    }

    @Test
    @Order(22)
    public void testParser_Expression_RightAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
        String sourceString = """
                foo:Integer
                bar:Integer
                baz:Integer
                1 >> foo
                2 >> bar
                3 >> baz
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), location(3, 0), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 5), 1);
        testSimpleRightAssignment(statements.get(4), location(4, 0), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 5), 2);
        testSimpleRightAssignment(statements.get(5), location(5, 0), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 5), 3);
    }

    @Test
    @Order(23)
    public void testParser_Expression_RightAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = """
                foo:Integer << 1
                bar:Integer << 2
                baz:Integer << 3
                4 >> foo
                5 >> bar
                6 >> baz
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(3), location(3, 0), "foo", VikariType.INTEGER, VikariType.INTEGER, location(3, 5), 4);
        testSimpleRightAssignment(statements.get(4), location(4, 0), "bar", VikariType.INTEGER, VikariType.INTEGER, location(4, 5), 5);
        testSimpleRightAssignment(statements.get(5), location(5, 0), "baz", VikariType.INTEGER, VikariType.INTEGER, location(5, 5), 6);
    }

    @Test
    @Order(24)
    public void testParser_Expression_RightAssignment_AllNumericTypes() {
        String sourceString = """
                a, b, c, d, e, f
                1 >> a
                2L >> b
                3B >> c
                4.0F >> d
                5.0D >> e
                6.0B >> f
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(6), location(1, 0), "a", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(1, 5), 1);

        testSimpleRightAssignment(statements.get(7), location(2, 0), "b", VikariType.ATONEMENT_CRYSTAL,
                VikariType.LONG, location(2, 6), 2L);

        testSimpleRightAssignment(statements.get(8), location(3, 0), "c", VikariType.ATONEMENT_CRYSTAL,
                VikariType.BIG_INTEGER, location(3, 6), new BigInteger("3"));

        testSimpleRightAssignment(statements.get(9), location(4, 0), "d", VikariType.ATONEMENT_CRYSTAL,
                VikariType.FLOAT, location(4, 8), 4.0F);

        testSimpleRightAssignment(statements.get(10), location(5, 0), "e", VikariType.ATONEMENT_CRYSTAL,
                VikariType.DOUBLE, location(5, 8), 5.0D);

        testSimpleRightAssignment(statements.get(11), location(6, 0), "f", VikariType.ATONEMENT_CRYSTAL,
                VikariType.BIG_DECIMAL, location(6, 8), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(25)
    public void testParser_Expression_RightAssignment_AllNumericTypes_WithTypeLabels() {
        String sourceString = """
                a:Integer
                b:Long
                c:BigInteger
                d:Float
                e:Double
                f:BigDecimal
                1 >> a
                2L >> b
                3B >> c
                4.0F >> d
                5.0D >> e
                6.0B >> f
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 12;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(4).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(5).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(6), location(6, 0), "a", VikariType.INTEGER,
                VikariType.INTEGER, location(6, 5), 1);

        testSimpleRightAssignment(statements.get(7), location(7, 0), "b", VikariType.LONG,
                VikariType.LONG, location(7, 6), 2L);

        testSimpleRightAssignment(statements.get(8), location(8, 0), "c", VikariType.BIG_INTEGER,
                VikariType.BIG_INTEGER, location(8, 6), new BigInteger("3"));

        testSimpleRightAssignment(statements.get(9), location(9, 0), "d", VikariType.FLOAT,
                VikariType.FLOAT, location(9, 8), 4.0F);

        testSimpleRightAssignment(statements.get(10), location(10, 0), "e", VikariType.DOUBLE,
                VikariType.DOUBLE, location(10, 8), 5.0D);

        testSimpleRightAssignment(statements.get(11), location(11, 0), "f", VikariType.BIG_DECIMAL,
                VikariType.BIG_DECIMAL, location(11, 8), new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(26)
    public void testParser_Expression_RightAssignment_NumericUpcasts() {
        String sourceString = """
                long1:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                float1:Float
                float2:Float
                float3:Float
                double1:Double
                double2:Double
                double3:Double
                double4:Double
                bigDecimal1:BigDecimal
                bigDecimal2:BigDecimal
                bigDecimal3:BigDecimal
                bigDecimal4:BigDecimal
                bigDecimal5:BigDecimal

                1 >> long1
                2 >> bigInteger1
                3L >> bigInteger2
                4 >> float1
                5L >> float2
                6B >> float3
                7 >> double1
                8L >> double2
                9B >> double3
                10.0F >> double4
                11 >> bigDecimal1
                12L >> bigDecimal2
                13B >> bigDecimal3
                14.0F >> bigDecimal4
                15.0D >> bigDecimal5
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }
        // assignment statements
        testSimpleRightAssignment(statements.get(15), location(16, 0), "long1", VikariType.LONG,
                VikariType.INTEGER, location(16, 5), 1);

        testSimpleRightAssignment(statements.get(16), location(17, 0), "bigInteger1", VikariType.BIG_INTEGER,
                VikariType.INTEGER, location(17, 5), 2);

        testSimpleRightAssignment(statements.get(17), location(18, 0), "bigInteger2", VikariType.BIG_INTEGER,
                VikariType.LONG, location(18, 6), 3L);

        testSimpleRightAssignment(statements.get(18), location(19, 0), "float1", VikariType.FLOAT,
                VikariType.INTEGER, location(19, 5), 4);

        testSimpleRightAssignment(statements.get(19), location(20, 0), "float2", VikariType.FLOAT,
                VikariType.LONG, location(20, 6), 5L);

        testSimpleRightAssignment(statements.get(20), location(21, 0), "float3", VikariType.FLOAT,
                VikariType.BIG_INTEGER, location(21, 6), new BigInteger("6"));

        testSimpleRightAssignment(statements.get(21), location(22, 0), "double1", VikariType.DOUBLE,
                VikariType.INTEGER, location(22, 5), 7);

        testSimpleRightAssignment(statements.get(22), location(23, 0), "double2", VikariType.DOUBLE,
                VikariType.LONG, location(23, 6), 8L);

        testSimpleRightAssignment(statements.get(23), location(24, 0), "double3", VikariType.DOUBLE,
                VikariType.BIG_INTEGER, location(24, 6), new BigInteger("9"));

        testSimpleRightAssignment(statements.get(24), location(25, 0), "double4", VikariType.DOUBLE,
                VikariType.FLOAT, location(25, 9), 10.0F);

        testSimpleRightAssignment(statements.get(25), location(26, 0), "bigDecimal1", VikariType.BIG_DECIMAL,
                VikariType.INTEGER, location(26, 6), 11);

        testSimpleRightAssignment(statements.get(26), location(27, 0), "bigDecimal2", VikariType.BIG_DECIMAL,
                VikariType.LONG, location(27, 7), 12L);

        testSimpleRightAssignment(statements.get(27), location(28, 0), "bigDecimal3", VikariType.BIG_DECIMAL,
                VikariType.BIG_INTEGER, location(28, 7), new BigInteger("13"));

        testSimpleRightAssignment(statements.get(28), location(29, 0), "bigDecimal4", VikariType.BIG_DECIMAL,
                VikariType.FLOAT, location(29, 9), 14.0F);

        testSimpleRightAssignment(statements.get(29), location(30, 0), "bigDecimal5", VikariType.BIG_DECIMAL,
                VikariType.DOUBLE, location(30, 9), 15.0D);
    }

    @Test
    @Order(27)
    public void testParser_Expression_RightAssignment_NumericDowncasts() {
        String sourceString = """
                integer1:Integer
                integer2:Integer
                integer3:Integer
                integer4:Integer
                integer5:Integer
                long1:Long
                long2:Long
                long3:Long
                long4:Long
                bigInteger1:BigInteger
                bigInteger2:BigInteger
                bigInteger3:BigInteger
                float1:Float
                float2:Float
                double1:Double

                1L >> integer1
                2B >> integer2
                3.0F >> integer3
                4.0D >> integer4
                5.0B >> integer5
                6B >> long1
                7.0F >> long2
                8.0D >> long3
                9.0B >> long4
                10.0F >> bigInteger1
                11.0D >> bigInteger2
                12.0B >> bigInteger3
                13.0D >> float1
                14.0B >> float2
                15.0B >> double1
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 30;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        for (int i = 0; i <= 14; i++) {
            assertEquals(VariableDeclarationStatement.class, statements.get(i).getClass(), "Unexpected statement type.");
        }

        MathContext mathContext = Arithmetic.getMathContext();

        // assignment statements
        testSimpleRightAssignment(statements.get(15), location(16, 0), "integer1", VikariType.INTEGER,
                VikariType.LONG, location(16, 6), 1L);

        testSimpleRightAssignment(statements.get(16), location(17, 0), "integer2", VikariType.INTEGER,
                VikariType.BIG_INTEGER, location(17, 6), new BigInteger("2"));

        testSimpleRightAssignment(statements.get(17), location(18, 0), "integer3", VikariType.INTEGER,
                VikariType.FLOAT, location(18, 8), 3.0F);

        testSimpleRightAssignment(statements.get(18), location(19, 0), "integer4", VikariType.INTEGER,
                VikariType.DOUBLE, location(19, 8), 4.0D);

        testSimpleRightAssignment(statements.get(19), location(20, 0), "integer5", VikariType.INTEGER,
                VikariType.BIG_DECIMAL, location(20, 8), new BigDecimal("5.0", mathContext));

        testSimpleRightAssignment(statements.get(20), location(21, 0), "long1", VikariType.LONG,
                VikariType.BIG_INTEGER, location(21, 6), new BigInteger("6"));

        testSimpleRightAssignment(statements.get(21), location(22, 0), "long2", VikariType.LONG,
                VikariType.FLOAT, location(22, 8), 7.0F);

        testSimpleRightAssignment(statements.get(22), location(23, 0), "long3", VikariType.LONG,
                VikariType.DOUBLE, location(23, 8), 8.0D);

        testSimpleRightAssignment(statements.get(23), location(24, 0), "long4", VikariType.LONG,
                VikariType.BIG_DECIMAL, location(24, 8), new BigDecimal("9.0", mathContext));

        testSimpleRightAssignment(statements.get(24), location(25, 0), "bigInteger1", VikariType.BIG_INTEGER,
                VikariType.FLOAT, location(25, 9), 10.0F);

        testSimpleRightAssignment(statements.get(25), location(26, 0), "bigInteger2", VikariType.BIG_INTEGER,
                VikariType.DOUBLE, location(26, 9), 11.0D);

        testSimpleRightAssignment(statements.get(26), location(27, 0), "bigInteger3", VikariType.BIG_INTEGER,
                VikariType.BIG_DECIMAL, location(27, 9), new BigDecimal("12.0", mathContext));

        testSimpleRightAssignment(statements.get(27), location(28, 0), "float1", VikariType.FLOAT,
                VikariType.DOUBLE, location(28, 9), 13.0D);

        testSimpleRightAssignment(statements.get(28), location(29, 0), "float2", VikariType.FLOAT,
                VikariType.BIG_DECIMAL, location(29, 9), new BigDecimal("14.0", mathContext));

        testSimpleRightAssignment(statements.get(29), location(30, 0), "double1", VikariType.DOUBLE,
                VikariType.BIG_DECIMAL, location(30, 9), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(28)
    public void testParser_Expression_RightAssignment_AssignmentToParentTypes() {
        String sourceString = """
                a, b:AtonementCrystal, c:Value, d:Number
                1 >> a
                2 >> b
                3 >> c
                4 >> d
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 8;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(4), location(1, 0), "a", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(1, 5), 1);

        testSimpleRightAssignment(statements.get(5), location(2, 0), "b", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(2, 5), 2);

        testSimpleRightAssignment(statements.get(6), location(3, 0), "c", VikariType.VALUE,
                VikariType.INTEGER, location(3, 5), 3);

        testSimpleRightAssignment(statements.get(7), location(4, 0), "d", VikariType.NUMBER,
                VikariType.INTEGER, location(4, 5), 4);
    }

    @Test
    @Order(29)
    public void testParser_Expression_RightAssignment_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                2 >> foo
                4 >> bar
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar", "Unknown Type.");

        int expectedStatementCount = 4;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment(statements.get(2), location(2, 0), "foo", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(2, 5), 2);

        testSimpleRightAssignment(statements.get(3), location(3, 0), "bar", VikariType.ATONEMENT_CRYSTAL,
                VikariType.INTEGER, location(3, 5), 4);
    }

    @Test
    @Order(30)
    public void testParser_Expression_RightAssignment_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type, 2 >> foo";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 15), "foo:Type, 2 >> foo", "Variable " +
                "with type Type cannot be assigned a value of type Integer.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // declaration statements
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");

        // assignment statements
        testSimpleRightAssignment_TypeError(statements.get(1), location(0, 10), "foo", VikariType.TYPE,
                VikariType.INTEGER, location(0, 15), 2);
    }

    // -----------------
    // More error cases.
    // -----------------

    @Test
    @Order(31)
    public void testParser_Expression_Assignment_SyntaxError_InvalidAssignmentTarget() {
        String sourceString = "[5 + 2] << 7, 7 >> [5 + 2]";

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 0), sourceString, "Invalid target for " +
                "assignment expression.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(0, 19), sourceString, "Invalid target for " +
                "assignment expression.");

        int expectedStatementCount = 2;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // left assignment statement: "[5 + 2] << 7"
        Statement statement = statements.get(0);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression innerExpression = expressionStatement.getExpression();
        assertEquals(LeftAssignmentExpression.class, innerExpression.getClass(), "Unexpected expression type.");
        LeftAssignmentExpression leftAssignmentExpression = (LeftAssignmentExpression) innerExpression;

        // erroneous lvalue (no need to assert further.)
        Expression lvalueExpression = leftAssignmentExpression.getLvalue();
        assertEquals(GroupingExpression.class, lvalueExpression.getClass(), "Unexpected expression type.");

        BinaryOperatorCrystal operator = leftAssignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        Expression rvalueExpression = leftAssignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        AtonementCrystal rvalue = ((LiteralExpression) rvalueExpression).getValue();
        TestUtils.testNumberCrystal(rvalue, 7, IntegerCrystal.class);

        // right assignment statement: "7 >> [5 + 2]"
        statement = statements.get(1);
        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        expressionStatement = (ExpressionStatement) statement;
        innerExpression = expressionStatement.getExpression();
        assertEquals(RightAssignmentExpression.class, innerExpression.getClass(), "Unexpected expression type.");
        RightAssignmentExpression rightAssignmentExpression = (RightAssignmentExpression) innerExpression;

        // erroneous lvalue (no need to assert further.)
        lvalueExpression = rightAssignmentExpression.getLvalue();
        assertEquals(GroupingExpression.class, lvalueExpression.getClass(), "Unexpected expression type.");

        operator = rightAssignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, operator.getClass(), "Unexpected operator type.");

        rvalueExpression = rightAssignmentExpression.getRvalue();
        assertEquals(LiteralExpression.class, rvalueExpression.getClass(), "Unexpected rvalue expression type.");

        rvalue = ((LiteralExpression) rvalueExpression).getValue();
        TestUtils.testNumberCrystal(rvalue, 7, IntegerCrystal.class);
    }

    // ---------------
    // Boolean values.
    // ---------------

    @Test
    @Order(32)
    public void testParser_Expression_LeftAssignment_BooleanValues() {
        String sourceString = """
                a
                b:AtonementCrystal
                c:Value
                d:Boolean

                a << true
                b << true
                c << true
                d << true
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 8;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // variable declarations
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");

        // left assignments
        testSimpleLeftAssignment(statements.get(4), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, location(5, 0), true);
        testSimpleLeftAssignment(statements.get(5), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, location(6, 0), true);
        testSimpleLeftAssignment(statements.get(6), "c", VikariType.VALUE, VikariType.BOOLEAN, location(7, 0), true);
        testSimpleLeftAssignment(statements.get(7), "d", VikariType.BOOLEAN, VikariType.BOOLEAN, location(8, 0), true);
    }

    @Test
    @Order(33)
    public void testParser_Expression_RightAssignment_BooleanValues() {
        String sourceString = """
                a
                b:AtonementCrystal
                c:Value
                d:Boolean

                false >> a
                false >> b
                false >> c
                false >> d
                """;
        List<Statement> statements = lexAndParse(sourceString);

        int expectedStatementCount = 8;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // variable declarations
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");

        // left assignments
        testSimpleRightAssignment(statements.get(4), location(5, 0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, location(5, 9), false);
        testSimpleRightAssignment(statements.get(5), location(6, 0), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, location(6, 9), false);
        testSimpleRightAssignment(statements.get(6), location(7, 0), "c", VikariType.VALUE, VikariType.BOOLEAN, location(7, 9), false);
        testSimpleRightAssignment(statements.get(7), location(8, 0), "d", VikariType.BOOLEAN, VikariType.BOOLEAN, location(8, 9), false);
    }

    @Test
    @Order(34)
    public void testParser_Expression_Assignment_SyntaxError_BooleanValues() {
        String sourceString = """
                foo:Integer
                foo << false
                true >> foo

                bar:Boolean
                bar << 2
                3.14 >> bar
                """;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 4);

        int expectedStatementCount = 6;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // variable declarations
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(3).getClass(), "Unexpected statement type.");

        // error assignments
        testSimpleLeftAssignment_TypeError(statements.get(1), "foo", VikariType.INTEGER, VikariType.BOOLEAN, location(1, 0), false);
        testSimpleRightAssignment_TypeError(statements.get(2), location(2, 0), "foo", VikariType.INTEGER, VikariType.BOOLEAN, location(2, 8), true);

        testSimpleLeftAssignment_TypeError(statements.get(4), "bar", VikariType.BOOLEAN, VikariType.INTEGER, location(5, 0), 2);
        testSimpleRightAssignment_TypeError(statements.get(5), location(6, 0), "bar", VikariType.BOOLEAN, VikariType.DOUBLE, location(6, 8), 3.14D);

        // syntax errors
        assertSyntaxErrors(syntaxErrorReporter, 4);
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();

        testSyntaxError(syntaxErrors.get(0), location(1, 0), "foo << false", "Variable with type Integer cannot be assigned a value of type Boolean.");
        testSyntaxError(syntaxErrors.get(1), location(2, 8), "true >> foo", "Variable with type Integer cannot be assigned a value of type Boolean.");
        testSyntaxError(syntaxErrors.get(2), location(5, 0), "bar << 2", "Variable with type Boolean cannot be assigned a value of type Integer.");
        testSyntaxError(syntaxErrors.get(3), location(6, 8), "3.14 >> bar", "Variable with type Boolean cannot be assigned a value of type Double.");
    }

    @Test
    @Order(35)
    public void testParser_Expression_Assignment_SyntaxError_BooleanValues_AssignmentFromVariable() {
        String sourceString = """
                bool:Boolean
                int:Integer << 2
                float:Float << 3.14F
                bool << int
                float >> bool
                """;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 2);

        int expectedStatementCount = 5;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // variable declarations
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // error assignments
        testLeftAssignment_FromVariable_TypeError(statements.get(3), "bool", VikariType.BOOLEAN, location(3, 0), "int",
                VikariType.INTEGER, VikariType.INTEGER, location(3, 8));

        testRightAssignment_FromVariable_TypeError(statements.get(4), "bool", VikariType.BOOLEAN, location(4, 9),
                "float", VikariType.FLOAT, VikariType.FLOAT, location(4, 0));

        // syntax errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();

        testSyntaxError(syntaxErrors.get(0), location(3, 0), "bool << int", "Variable with type Boolean cannot be assigned a value of type Integer.");
        testSyntaxError(syntaxErrors.get(1), location(4, 9), "float >> bool", "Variable with type Boolean cannot be assigned a value of type Float.");
    }

    @Test
    @Order(36)
    public void testParser_Expression_Assignment_SyntaxError_BooleanValues_AssignmentFromBooleanVariable() {
        String sourceString = """
                bool:Boolean << true
                int:Integer
                float:Float
                int << bool
                bool >> float
                """;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, 2);

        int expectedStatementCount = 5;
        int actualStatementCount = statements.size();
        assertEquals(expectedStatementCount, actualStatementCount, "Unexpected statement count.");

        // variable declarations
        assertEquals(VariableDeclarationStatement.class, statements.get(0).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(1).getClass(), "Unexpected statement type.");
        assertEquals(VariableDeclarationStatement.class, statements.get(2).getClass(), "Unexpected statement type.");

        // error assignments
        testLeftAssignment_FromVariable_TypeError(statements.get(3), "int", VikariType.INTEGER, location(3, 0), "bool",
                VikariType.BOOLEAN, VikariType.BOOLEAN, location(3, 7));

        testRightAssignment_FromVariable_TypeError(statements.get(4), "float", VikariType.FLOAT, location(4, 8),
                "bool", VikariType.BOOLEAN, VikariType.BOOLEAN, location(4, 0));

        // syntax errors
        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();

        testSyntaxError(syntaxErrors.get(0), location(3, 0), "int << bool", "Variable with type Integer cannot be assigned a value of type Boolean.");
        testSyntaxError(syntaxErrors.get(1), location(4, 8), "bool >> float", "Variable with type Float cannot be assigned a value of type Boolean.");
    }


    @Test
    @Order(37)
    public void testParser_Expression_LeftAssignment_Nulls_LengthZero() {
        String sourceString = """
                a, b
                a << null, b << _[0]_

                c:Integer, d:Integer
                c << null, d << __[0]__
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 3));

        testLeftAssignment_NullKeyword(statements.get(2), "a", VikariType.ATONEMENT_CRYSTAL, location(1, 0),
                location(1, 5));
        testLeftAssignment_NullLiteralExpression(statements.get(3), "b", VikariType.ATONEMENT_CRYSTAL, location(1, 11),
                location(1, 16), location(1, 18), 0);

        testDeclaration(statements.get(4), "c", VikariType.INTEGER, VikariType.NULL, location(3, 0));
        testDeclaration(statements.get(5), "d", VikariType.INTEGER, VikariType.NULL, location(3, 11));

        testLeftAssignment_NullKeyword(statements.get(6), "c", VikariType.INTEGER, location(4, 0),
                location(4, 5));
        testLeftAssignment_NullLiteralExpression(statements.get(7), "d", VikariType.INTEGER, location(4, 11),
                location(4, 16), location(4, 19), 0);
    }

    @Test
    @Order(38)
    public void testParser_Expression_LeftAssignment_Nulls_PositiveLength() {
        String sourceString = """
                a, b, c, d, e

                a << _
                b << __
                c << ___
                d << ____
                e << _____

                f, g, h, i, j

                f << _[1]_
                g << _[2]_
                h << _[3]_
                i << _[4]_
                j << _[5]_

                k:Integer
                l:Integer
                m:Integer
                n:Integer
                o:Integer

                k << _
                l << __
                m << ___
                n << ____
                o << _____

                p:Integer
                q:Integer
                r:Integer
                s:Integer
                t:Integer

                p << _[1]_
                q << _[2]_
                r << _[3]_
                s << _[4]_
                t << _[5]_
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 3));
        testDeclaration(statements.get(2), "c", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 6));
        testDeclaration(statements.get(3), "d", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 9));
        testDeclaration(statements.get(4), "e", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 12));

        testLeftAssignment_NullSwordLiteral(statements.get(5), "a", VikariType.ATONEMENT_CRYSTAL, location(2, 0),
                location(2, 5), 1);
        testLeftAssignment_NullSwordLiteral(statements.get(6), "b", VikariType.ATONEMENT_CRYSTAL, location(3, 0),
                location(3, 5), 2);
        testLeftAssignment_NullSwordLiteral(statements.get(7), "c", VikariType.ATONEMENT_CRYSTAL, location(4, 0),
                location(4, 5), 3);
        testLeftAssignment_NullSwordLiteral(statements.get(8), "d", VikariType.ATONEMENT_CRYSTAL, location(5, 0),
                location(5, 5), 4);
        testLeftAssignment_NullSwordLiteral(statements.get(9), "e", VikariType.ATONEMENT_CRYSTAL, location(6, 0),
                location(6, 5), 5);

        testDeclaration(statements.get(10), "f", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 0));
        testDeclaration(statements.get(11), "g", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 3));
        testDeclaration(statements.get(12), "h", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 6));
        testDeclaration(statements.get(13), "i", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 9));
        testDeclaration(statements.get(14), "j", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 12));

        testLeftAssignment_NullLiteralExpression(statements.get(15), "f", VikariType.ATONEMENT_CRYSTAL, location(10, 0),
                location(10, 5), location(10, 7), 1);
        testLeftAssignment_NullLiteralExpression(statements.get(16), "g", VikariType.ATONEMENT_CRYSTAL, location(11, 0),
                location(11, 5), location(11, 7), 2);
        testLeftAssignment_NullLiteralExpression(statements.get(17), "h", VikariType.ATONEMENT_CRYSTAL, location(12, 0),
                location(12, 5), location(12, 7), 3);
        testLeftAssignment_NullLiteralExpression(statements.get(18), "i", VikariType.ATONEMENT_CRYSTAL, location(13, 0),
                location(13, 5), location(13, 7), 4);
        testLeftAssignment_NullLiteralExpression(statements.get(19), "j", VikariType.ATONEMENT_CRYSTAL, location(14, 0),
                location(14, 5), location(14, 7), 5);

        testDeclaration(statements.get(20), "k", VikariType.INTEGER, VikariType.NULL, location(16, 0));
        testDeclaration(statements.get(21), "l", VikariType.INTEGER, VikariType.NULL, location(17, 0));
        testDeclaration(statements.get(22), "m", VikariType.INTEGER, VikariType.NULL, location(18, 0));
        testDeclaration(statements.get(23), "n", VikariType.INTEGER, VikariType.NULL, location(19, 0));
        testDeclaration(statements.get(24), "o", VikariType.INTEGER, VikariType.NULL, location(20, 0));

        testLeftAssignment_NullSwordLiteral(statements.get(25), "k", VikariType.INTEGER, location(22, 0),
                location(22, 5), 1);
        testLeftAssignment_NullSwordLiteral(statements.get(26), "l", VikariType.INTEGER, location(23, 0),
                location(23, 5), 2);
        testLeftAssignment_NullSwordLiteral(statements.get(27), "m", VikariType.INTEGER, location(24, 0),
                location(24, 5), 3);
        testLeftAssignment_NullSwordLiteral(statements.get(28), "n", VikariType.INTEGER, location(25, 0),
                location(25, 5), 4);
        testLeftAssignment_NullSwordLiteral(statements.get(29), "o", VikariType.INTEGER, location(26, 0),
                location(26, 5), 5);

        testDeclaration(statements.get(30), "p", VikariType.INTEGER, VikariType.NULL, location(28, 0));
        testDeclaration(statements.get(31), "q", VikariType.INTEGER, VikariType.NULL, location(29, 0));
        testDeclaration(statements.get(32), "r", VikariType.INTEGER, VikariType.NULL, location(30, 0));
        testDeclaration(statements.get(33), "s", VikariType.INTEGER, VikariType.NULL, location(31, 0));
        testDeclaration(statements.get(34), "t", VikariType.INTEGER, VikariType.NULL, location(32, 0));

        testLeftAssignment_NullLiteralExpression(statements.get(35), "p", VikariType.INTEGER, location(34, 0),
                location(34, 5), location(34, 7), 1);
        testLeftAssignment_NullLiteralExpression(statements.get(36), "q", VikariType.INTEGER, location(35, 0),
                location(35, 5), location(35, 7), 2);
        testLeftAssignment_NullLiteralExpression(statements.get(37), "r", VikariType.INTEGER, location(36, 0),
                location(36, 5), location(36, 7), 3);
        testLeftAssignment_NullLiteralExpression(statements.get(38), "s", VikariType.INTEGER, location(37, 0),
                location(37, 5), location(37, 7), 4);
        testLeftAssignment_NullLiteralExpression(statements.get(39), "t", VikariType.INTEGER, location(38, 0),
                location(38, 5), location(38, 7), 5);
    }

    @Test
    @Order(39)
    public void testParser_Expression_LeftAssignment_Nulls_NegativeLength() {
        String sourceString = """
                a, b, c, d, e

                a << _[-1]_
                b << _[-2]_
                c << _[-3]_
                d << _[-4]_
                e << _[-5]_

                f:Integer
                g:Integer
                h:Integer
                i:Integer
                j:Integer

                f << _[-1]_
                g << _[-2]_
                h << _[-3]_
                i << _[-4]_
                j << _[-5]_
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 3));
        testDeclaration(statements.get(2), "c", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 6));
        testDeclaration(statements.get(3), "d", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 9));
        testDeclaration(statements.get(4), "e", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 12));

        testLeftAssignment_NullLiteralExpression(statements.get(5), "a", VikariType.ATONEMENT_CRYSTAL, location(2, 0),
                location(2, 5), location(2, 8), -1);
        testLeftAssignment_NullLiteralExpression(statements.get(6), "b", VikariType.ATONEMENT_CRYSTAL, location(3, 0),
                location(3, 5), location(3, 8), -2);
        testLeftAssignment_NullLiteralExpression(statements.get(7), "c", VikariType.ATONEMENT_CRYSTAL, location(4, 0),
                location(4, 5), location(4, 8), -3);
        testLeftAssignment_NullLiteralExpression(statements.get(8), "d", VikariType.ATONEMENT_CRYSTAL, location(5, 0),
                location(5, 5), location(5, 8), -4);
        testLeftAssignment_NullLiteralExpression(statements.get(9), "e", VikariType.ATONEMENT_CRYSTAL, location(6, 0),
                location(6, 5), location(6, 8), -5);

        testDeclaration(statements.get(10), "f", VikariType.INTEGER, VikariType.NULL, location(8, 0));
        testDeclaration(statements.get(11), "g", VikariType.INTEGER, VikariType.NULL, location(9, 0));
        testDeclaration(statements.get(12), "h", VikariType.INTEGER, VikariType.NULL, location(10, 0));
        testDeclaration(statements.get(13), "i", VikariType.INTEGER, VikariType.NULL, location(11, 0));
        testDeclaration(statements.get(14), "j", VikariType.INTEGER, VikariType.NULL, location(12, 0));

        testLeftAssignment_NullLiteralExpression(statements.get(15), "f", VikariType.INTEGER, location(14, 0),
                location(14, 5), location(14, 8), -1);
        testLeftAssignment_NullLiteralExpression(statements.get(16), "g", VikariType.INTEGER, location(15, 0),
                location(15, 5), location(15, 8), -2);
        testLeftAssignment_NullLiteralExpression(statements.get(17), "h", VikariType.INTEGER, location(16, 0),
                location(16, 5), location(16, 8), -3);
        testLeftAssignment_NullLiteralExpression(statements.get(18), "i", VikariType.INTEGER, location(17, 0),
                location(17, 5), location(17, 8), -4);
        testLeftAssignment_NullLiteralExpression(statements.get(19), "j", VikariType.INTEGER, location(18, 0),
                location(18, 5), location(18, 8), -5);
    }

    @Test
    @Order(40)
    public void testParser_Expression_LeftAssignment_Nulls_NullLiteralExpression_WithVariable() {
        String sourceString = """
                int:Integer << 2, foo
                foo << __[int]__
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "int", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2);
        testDeclaration(statements.get(1), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 18));

        // null literal expression with variable
        Statement statement = statements.get(2);
        assertEquals(location(1, 0), statement.getLocation(), "Unexpected location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression expression = expressionStatement.getExpression();

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected expression type.");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(1, 0));

        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression rvalue = assignmentExpression.getRvalue();
        assertEquals(location(1, 7), rvalue.getLocation(), "Unexpected location.");
        assertEquals(NullLiteralExpression.class, rvalue.getClass(), "Unexpected expression type.");

        NullLiteralExpression nullLiteralExpression = (NullLiteralExpression) rvalue;
        Expression innerExpression = nullLiteralExpression.getExpression();
        assertEquals(VariableExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        VariableExpression variableExpression = (VariableExpression) innerExpression;
        AtonementCrystal reference = variableExpression.getReference();
        testVariableCrystal(reference, "int", VikariType.INTEGER, VikariType.INTEGER, location(1, 10));
    }

    @Test
    @Order(41)
    public void testParser_Expression_LeftAssignment_Nulls_NullLiteralExpression_WithArithmeticExpression() {
        String sourceString = """
                foo, foo << __[5 + 7]__
                """;
        List<Statement> statements = lexAndParse(sourceString);
        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));

        // null literal expression with arithmetic expression
        Statement statement = statements.get(1);
        assertEquals(location(0, 5), statement.getLocation(), "Unexpected location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression expression = expressionStatement.getExpression();

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected expression type.");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 5));

        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression rvalue = assignmentExpression.getRvalue();
        assertEquals(location(0, 12), rvalue.getLocation(), "Unexpected location.");
        assertEquals(NullLiteralExpression.class, rvalue.getClass(), "Unexpected expression type.");
        NullLiteralExpression nullLiteralExpression = (NullLiteralExpression) rvalue;

        Expression innerExpression = nullLiteralExpression.getExpression();
        assertEquals(location(0, 15), innerExpression.getLocation(), "Unexpected location.");
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

        assertEquals(location(0, 15), leftOperand.getCoordinates(), "Unexpected location.");
        assertEquals(location(0, 17), binaryOperator.getCoordinates(), "Unexpected location.");
        assertEquals(location(0, 19), rightOperand.getCoordinates(), "Unexpected location.");

        testNumberCrystal(leftOperand, 5, IntegerCrystal.class);
        testNumberCrystal(rightOperand, 7, IntegerCrystal.class);
    }

    @Test
    @Order(42)
    public void testParser_Expression_LeftAssignment_Nulls_FromVariable() {
        String sourceString = """
                foo:Integer
                bar:Integer << 2
                bar << foo
                """;

        List<Statement> statements = lexAndParse(sourceString);
        testDeclaration(statements.get(0), "foo", VikariType.INTEGER, VikariType.NULL, location(0, 0));

        // NOTE: As the TypeResolver updates the instantiatedType in this crystal's field at the assignment expression
        //       in statement 3, it is now Null and not Integer. And so, the declaration must be tested separately to
        //       account for this discrepancy.

        Statement statement = statements.get(1);
        assertEquals(location(1, 0), statement.getLocation(), "Unexpected location.");
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, "bar", VikariType.INTEGER, VikariType.NULL, location(1, 0));

        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        ParserTest_Utils.testRvalue(2, literal, VikariType.INTEGER);

        // variable assignment expression
        statement = statements.get(2);
        assertEquals(location(2, 0), statement.getLocation(), "Unexpected location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression expression = expressionStatement.getExpression();

        assertEquals(LeftAssignmentExpression.class, expression.getClass(), "Unexpected expression type.");
        LeftAssignmentExpression assignmentExpression = (LeftAssignmentExpression) expression;

        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal lvalueVariable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(lvalueVariable, "bar", VikariType.INTEGER, VikariType.NULL, location(2, 0));

        assignmentOperator = assignmentExpression.getOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression rvalue = assignmentExpression.getRvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal rvalueVariable = ((VariableExpression) rvalue).getReference();
        testVariableCrystal(rvalueVariable, "foo", VikariType.INTEGER, VikariType.NULL, location(2, 7));
    }

    @Test
    @Order(43)
    public void testParser_Expression_RightAssignment_Nulls_LengthZero() {
        String sourceString = """
                a, b
                null >> a, _[0]_ >> b

                c:Integer, d:Integer
                null >> c, __[0]__ >> d
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 3));

        testRightAssignment_NullKeyword(statements.get(2), "a", VikariType.ATONEMENT_CRYSTAL, location(1, 8),
                location(1, 0));
        testRightAssignment_NullLiteralExpression(statements.get(3), "b", VikariType.ATONEMENT_CRYSTAL, location(1, 20),
                location(1, 11), location(1, 13), 0);

        testDeclaration(statements.get(4), "c", VikariType.INTEGER, VikariType.NULL, location(3, 0));
        testDeclaration(statements.get(5), "d", VikariType.INTEGER, VikariType.NULL, location(3, 11));

        testRightAssignment_NullKeyword(statements.get(6), "c", VikariType.INTEGER, location(4, 8),
                location(4, 0));
        testRightAssignment_NullLiteralExpression(statements.get(7), "d", VikariType.INTEGER, location(4, 22),
                location(4, 11), location(4, 14), 0);
    }

    @Test
    @Order(44)
    public void testParser_Expression_RightAssignment_Nulls_PositiveLength() {
        String sourceString = """
                a, b, c, d, e

                _ >> a
                __ >> b
                ___ >> c
                ____ >> d
                _____ >> e

                f, g, h, i, j

                _[1]_ >> f
                _[2]_ >> g
                _[3]_ >> h
                _[4]_ >> i
                _[5]_ >> j

                k:Integer
                l:Integer
                m:Integer
                n:Integer
                o:Integer

                _ >> k
                __ >> l
                ___ >> m
                ____ >> n
                _____ >> o

                p:Integer
                q:Integer
                r:Integer
                s:Integer
                t:Integer

                _[1]_ >> p
                _[2]_ >> q
                _[3]_ >> r
                _[4]_ >> s
                _[5]_ >> t
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 3));
        testDeclaration(statements.get(2), "c", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 6));
        testDeclaration(statements.get(3), "d", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 9));
        testDeclaration(statements.get(4), "e", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 12));

        testRightAssignment_NullSwordLiteral(statements.get(5), "a", VikariType.ATONEMENT_CRYSTAL, location(2, 5),
                location(2, 0), 1);
        testRightAssignment_NullSwordLiteral(statements.get(6), "b", VikariType.ATONEMENT_CRYSTAL, location(3, 6),
                location(3, 0), 2);
        testRightAssignment_NullSwordLiteral(statements.get(7), "c", VikariType.ATONEMENT_CRYSTAL, location(4, 7),
                location(4, 0), 3);
        testRightAssignment_NullSwordLiteral(statements.get(8), "d", VikariType.ATONEMENT_CRYSTAL, location(5, 8),
                location(5, 0), 4);
        testRightAssignment_NullSwordLiteral(statements.get(9), "e", VikariType.ATONEMENT_CRYSTAL, location(6, 9),
                location(6, 0), 5);

        testDeclaration(statements.get(10), "f", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 0));
        testDeclaration(statements.get(11), "g", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 3));
        testDeclaration(statements.get(12), "h", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 6));
        testDeclaration(statements.get(13), "i", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 9));
        testDeclaration(statements.get(14), "j", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(8, 12));

        testRightAssignment_NullLiteralExpression(statements.get(15), "f", VikariType.ATONEMENT_CRYSTAL,
                location(10, 9), location(10, 0), location(10, 2), 1);
        testRightAssignment_NullLiteralExpression(statements.get(16), "g", VikariType.ATONEMENT_CRYSTAL,
                location(11, 9), location(11, 0), location(11, 2), 2);
        testRightAssignment_NullLiteralExpression(statements.get(17), "h", VikariType.ATONEMENT_CRYSTAL,
                location(12, 9), location(12, 0), location(12, 2), 3);
        testRightAssignment_NullLiteralExpression(statements.get(18), "i", VikariType.ATONEMENT_CRYSTAL,
                location(13, 9), location(13, 0), location(13, 2), 4);
        testRightAssignment_NullLiteralExpression(statements.get(19), "j", VikariType.ATONEMENT_CRYSTAL,
                location(14, 9), location(14, 0), location(14, 2), 5);

        testDeclaration(statements.get(20), "k", VikariType.INTEGER, VikariType.NULL, location(16, 0));
        testDeclaration(statements.get(21), "l", VikariType.INTEGER, VikariType.NULL, location(17, 0));
        testDeclaration(statements.get(22), "m", VikariType.INTEGER, VikariType.NULL, location(18, 0));
        testDeclaration(statements.get(23), "n", VikariType.INTEGER, VikariType.NULL, location(19, 0));
        testDeclaration(statements.get(24), "o", VikariType.INTEGER, VikariType.NULL, location(20, 0));

        testRightAssignment_NullSwordLiteral(statements.get(25), "k", VikariType.INTEGER, location(22, 5),
                location(22, 0), 1);
        testRightAssignment_NullSwordLiteral(statements.get(26), "l", VikariType.INTEGER, location(23, 6),
                location(23, 0), 2);
        testRightAssignment_NullSwordLiteral(statements.get(27), "m", VikariType.INTEGER, location(24, 7),
                location(24, 0), 3);
        testRightAssignment_NullSwordLiteral(statements.get(28), "n", VikariType.INTEGER, location(25, 8),
                location(25, 0), 4);
        testRightAssignment_NullSwordLiteral(statements.get(29), "o", VikariType.INTEGER, location(26, 9),
                location(26, 0), 5);

        testDeclaration(statements.get(30), "p", VikariType.INTEGER, VikariType.NULL, location(28, 0));
        testDeclaration(statements.get(31), "q", VikariType.INTEGER, VikariType.NULL, location(29, 0));
        testDeclaration(statements.get(32), "r", VikariType.INTEGER, VikariType.NULL, location(30, 0));
        testDeclaration(statements.get(33), "s", VikariType.INTEGER, VikariType.NULL, location(31, 0));
        testDeclaration(statements.get(34), "t", VikariType.INTEGER, VikariType.NULL, location(32, 0));

        testRightAssignment_NullLiteralExpression(statements.get(35), "p", VikariType.INTEGER, location(34, 9),
                location(34, 0), location(34, 2), 1);
        testRightAssignment_NullLiteralExpression(statements.get(36), "q", VikariType.INTEGER, location(35, 9),
                location(35, 0), location(35, 2), 2);
        testRightAssignment_NullLiteralExpression(statements.get(37), "r", VikariType.INTEGER, location(36, 9),
                location(36, 0), location(36, 2), 3);
        testRightAssignment_NullLiteralExpression(statements.get(38), "s", VikariType.INTEGER, location(37, 9),
                location(37, 0), location(37, 2), 4);
        testRightAssignment_NullLiteralExpression(statements.get(39), "t", VikariType.INTEGER, location(38, 9),
                location(38, 0), location(38, 2), 5);
    }

    @Test
    @Order(45)
    public void testParser_Expression_RightAssignment_Nulls_NegativeLength() {
        String sourceString = """
                a, b, c, d, e

                _[-1]_ >> a
                _[-2]_ >> b
                _[-3]_ >> c
                _[-4]_ >> d
                _[-5]_ >> e

                f:Integer
                g:Integer
                h:Integer
                i:Integer
                j:Integer

                _[-1]_ >> f
                _[-2]_ >> g
                _[-3]_ >> h
                _[-4]_ >> i
                _[-5]_ >> j
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 3));
        testDeclaration(statements.get(2), "c", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 6));
        testDeclaration(statements.get(3), "d", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 9));
        testDeclaration(statements.get(4), "e", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 12));

        testRightAssignment_NullLiteralExpression(statements.get(5), "a", VikariType.ATONEMENT_CRYSTAL, location(2, 10),
                location(2, 0), location(2, 3), -1);
        testRightAssignment_NullLiteralExpression(statements.get(6), "b", VikariType.ATONEMENT_CRYSTAL, location(3, 10),
                location(3, 0), location(3, 3), -2);
        testRightAssignment_NullLiteralExpression(statements.get(7), "c", VikariType.ATONEMENT_CRYSTAL, location(4, 10),
                location(4, 0), location(4, 3), -3);
        testRightAssignment_NullLiteralExpression(statements.get(8), "d", VikariType.ATONEMENT_CRYSTAL, location(5, 10),
                location(5, 0), location(5, 3), -4);
        testRightAssignment_NullLiteralExpression(statements.get(9), "e", VikariType.ATONEMENT_CRYSTAL, location(6, 10),
                location(6, 0), location(6, 3), -5);

        testDeclaration(statements.get(10), "f", VikariType.INTEGER, VikariType.NULL, location(8, 0));
        testDeclaration(statements.get(11), "g", VikariType.INTEGER, VikariType.NULL, location(9, 0));
        testDeclaration(statements.get(12), "h", VikariType.INTEGER, VikariType.NULL, location(10, 0));
        testDeclaration(statements.get(13), "i", VikariType.INTEGER, VikariType.NULL, location(11, 0));
        testDeclaration(statements.get(14), "j", VikariType.INTEGER, VikariType.NULL, location(12, 0));

        testRightAssignment_NullLiteralExpression(statements.get(15), "f", VikariType.INTEGER, location(14, 10),
                location(14, 0), location(14, 3), -1);
        testRightAssignment_NullLiteralExpression(statements.get(16), "g", VikariType.INTEGER, location(15, 10),
                location(15, 0), location(15, 3), -2);
        testRightAssignment_NullLiteralExpression(statements.get(17), "h", VikariType.INTEGER, location(16, 10),
                location(16, 0), location(16, 3), -3);
        testRightAssignment_NullLiteralExpression(statements.get(18), "i", VikariType.INTEGER, location(17, 10),
                location(17, 0), location(17, 3), -4);
        testRightAssignment_NullLiteralExpression(statements.get(19), "j", VikariType.INTEGER, location(18, 10),
                location(18, 0), location(18, 3), -5);
    }

    @Test
    @Order(46)
    public void testParser_Expression_RightAssignment_Nulls_NullLiteralExpression_WithVariable() {
        String sourceString = """
                int:Integer << 2, foo
                __[int]__ >> foo
                """;
        List<Statement> statements = lexAndParse(sourceString);

        testDeclaration(statements.get(0), "int", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2);
        testDeclaration(statements.get(1), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 18));

        // null literal expression with variable
        Statement statement = statements.get(2);
        assertEquals(location(1, 0), statement.getLocation(), "Unexpected location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression expression = expressionStatement.getExpression();

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected expression type.");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(1, 13));

        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression rvalue = assignmentExpression.getRvalue();
        assertEquals(location(1, 0), rvalue.getLocation(), "Unexpected location.");
        assertEquals(NullLiteralExpression.class, rvalue.getClass(), "Unexpected expression type.");

        NullLiteralExpression nullLiteralExpression = (NullLiteralExpression) rvalue;
        Expression innerExpression = nullLiteralExpression.getExpression();
        assertEquals(VariableExpression.class, innerExpression.getClass(), "Unexpected expression type.");

        VariableExpression variableExpression = (VariableExpression) innerExpression;
        AtonementCrystal reference = variableExpression.getReference();
        testVariableCrystal(reference, "int", VikariType.INTEGER, VikariType.INTEGER, location(1, 3));
    }

    @Test
    @Order(47)
    public void testParser_Expression_RightAssignment_Nulls_NullLiteralExpression_WithArithmeticExpression() {
        String sourceString = """
                foo, __[5 + 7]__ >> foo
                """;
        List<Statement> statements = lexAndParse(sourceString);
        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 0));

        // null literal expression with arithmetic expression
        Statement statement = statements.get(1);
        assertEquals(location(0, 5), statement.getLocation(), "Unexpected location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression expression = expressionStatement.getExpression();

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected expression type.");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal variable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(variable, "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.NULL, location(0, 20));

        BinaryOperatorCrystal assignmentOperator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression rvalue = assignmentExpression.getRvalue();
        assertEquals(location(0, 5), rvalue.getLocation(), "Unexpected location.");
        assertEquals(NullLiteralExpression.class, rvalue.getClass(), "Unexpected expression type.");
        NullLiteralExpression nullLiteralExpression = (NullLiteralExpression) rvalue;

        Expression innerExpression = nullLiteralExpression.getExpression();
        assertEquals(location(0, 8), innerExpression.getLocation(), "Unexpected location.");
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

        assertEquals(location(0, 8), leftOperand.getCoordinates(), "Unexpected location.");
        assertEquals(location(0, 10), binaryOperator.getCoordinates(), "Unexpected location.");
        assertEquals(location(0, 12), rightOperand.getCoordinates(), "Unexpected location.");

        testNumberCrystal(leftOperand, 5, IntegerCrystal.class);
        testNumberCrystal(rightOperand, 7, IntegerCrystal.class);
    }

    @Test
    @Order(48)
    public void testParser_Expression_RightAssignment_Nulls_FromVariable() {
        String sourceString = """
                foo:Integer
                bar:Integer << 2
                foo >> bar
                """;

        List<Statement> statements = lexAndParse(sourceString);
        testDeclaration(statements.get(0), "foo", VikariType.INTEGER, VikariType.NULL, location(0, 0));

        // NOTE: As the TypeResolver updates the instantiatedType in this crystal's field at the assignment expression
        //       in statement 3, it is now Null and not Integer. And so, the declaration must be tested separately to
        //       account for this discrepancy.

        Statement statement = statements.get(1);
        assertEquals(location(1, 0), statement.getLocation(), "Unexpected location.");
        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, "bar", VikariType.INTEGER, VikariType.NULL, location(1, 0));

        BinaryOperatorCrystal assignmentOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        ParserTest_Utils.testRvalue(2, literal, VikariType.INTEGER);

        // variable assignment expression
        statement = statements.get(2);
        assertEquals(location(2, 0), statement.getLocation(), "Unexpected location.");

        assertEquals(ExpressionStatement.class, statement.getClass(), "Unexpected statement type.");
        ExpressionStatement expressionStatement = (ExpressionStatement) statement;
        Expression expression = expressionStatement.getExpression();

        assertEquals(RightAssignmentExpression.class, expression.getClass(), "Unexpected expression type.");
        RightAssignmentExpression assignmentExpression = (RightAssignmentExpression) expression;

        Expression lvalue = assignmentExpression.getLvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal lvalueVariable = ((VariableExpression) lvalue).getReference();
        testVariableCrystal(lvalueVariable, "bar", VikariType.INTEGER, VikariType.NULL, location(2, 7));

        assignmentOperator = assignmentExpression.getOperator();
        assertEquals(RightAssignmentOperatorCrystal.class, assignmentOperator.getClass(), "Unexpected operator type.");

        Expression rvalue = assignmentExpression.getRvalue();
        assertEquals(VariableExpression.class, lvalue.getClass(), "Unexpected expression type.");
        AtonementCrystal rvalueVariable = ((VariableExpression) rvalue).getReference();
        testVariableCrystal(rvalueVariable, "foo", VikariType.INTEGER, VikariType.NULL, location(2, 0));
    }
}
