package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_VariableDeclarationStatements extends TreeWalkInterpreterTest_Base {

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_VariableDeclaration_Basic() {
        String sourceString = "foo";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_VariableDeclaration_WithTypeLabel() {
        String sourceString = "foo:Integer";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_VariableDeclaration_WithAssignment() {
        String sourceString = "foo << 2";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_VariableDeclaration_WithTypeLabel_AndAssignment() {
        String sourceString = "foo:Integer << 2";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 2);
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SingleLine() {
        String sourceString = "foo,bar,baz";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL);
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SeparateLines() {
        String sourceString = "foo\nbar\nbaz";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL);
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SingleLine_WithTypeLabels() {
        String sourceString = "foo:Integer,bar:Integer,baz:Integer";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER);
        testVariable("bar", VikariType.INTEGER);
        testVariable("baz", VikariType.INTEGER);
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SeparateLines_WithTypeLabels() {
        String sourceString = "foo:Integer\nbar:Integer\nbaz:Integer";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER);
        testVariable("bar", VikariType.INTEGER);
        testVariable("baz", VikariType.INTEGER);
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SingleLine_WithInitializerExpressions() {
        String sourceString = "foo << 2, bar << 3, baz << 4";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 3);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 4);
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SeparateLines_WithInitializerExpressions() {
        String sourceString = "foo << 2\nbar << 3\nbaz << 4";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 3);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 4);
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SingleLine_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = "foo:Integer << 2, bar:Integer << 3, baz:Integer << 4";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 3);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 4);
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_VariableDeclaration_Multiple_SeparateLines_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = "foo:Integer << 2\nbar:Integer << 3\nbaz:Integer << 4";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 3);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 4);
    }

    @Test
    @Order(13)
    public void testTreeWalkInterpreter_VariableDeclaration_AllNumericTypes() {
        String sourceString = """
                a:Integer
                b:Long
                c:BigInteger
                d:Float
                e:Double
                f:BigDecimal
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.INTEGER);
        testVariable("b", VikariType.LONG);
        testVariable("c", VikariType.BIG_INTEGER);
        testVariable("d", VikariType.FLOAT);
        testVariable("e", VikariType.DOUBLE);
        testVariable("f", VikariType.BIG_DECIMAL);
    }

    @Test
    @Order(14)
    public void testTreeWalkInterpreter_VariableDeclaration_AllNumericTypes_WithInitializer() {
        String sourceString = """
                a:Integer << 1
                b:Long << 2L
                c:BigInteger << 3B
                d:Float << 4.0F
                e:Double << 5.0D
                f:BigDecimal << 6.0B
                """;
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("b", VikariType.LONG, VikariType.LONG, 2L);
        testVariable("c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.DOUBLE, VikariType.DOUBLE, 5.0D);
        testVariable("f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("6.0", mathContext));
    }

    @Test
    @Order(15)
    public void testTreeWalkInterpreter_VariableDeclaration_NumericUpcasts() {
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
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("a", VikariType.LONG, VikariType.LONG, 1L);
        testVariable("b", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("2"));
        testVariable("c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.FLOAT, VikariType.FLOAT, 5.0F);
        testVariable("f", VikariType.FLOAT, VikariType.FLOAT, 6.0F);
        testVariable("g", VikariType.DOUBLE, VikariType.DOUBLE, 7.0D);
        testVariable("h", VikariType.DOUBLE, VikariType.DOUBLE, 8.0D);
        testVariable("i", VikariType.DOUBLE, VikariType.DOUBLE, 9.0D);
        testVariable("j", VikariType.DOUBLE, VikariType.DOUBLE, 10.0D);
        testVariable("k", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("11", mathContext));
        testVariable("l", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("12", mathContext));
        testVariable("m", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("13", mathContext));
        testVariable("n", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("14.0", mathContext));
        testVariable("o", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(16)
    public void testTreeWalkInterpreter_VariableDeclaration_NumericDowncasts() {
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
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("c", VikariType.INTEGER, VikariType.INTEGER, 3);
        testVariable("d", VikariType.INTEGER, VikariType.INTEGER, 4);
        testVariable("e", VikariType.INTEGER, VikariType.INTEGER, 5);
        testVariable("f", VikariType.LONG, VikariType.LONG, 6L);
        testVariable("g", VikariType.LONG, VikariType.LONG, 7L);
        testVariable("h", VikariType.LONG, VikariType.LONG, 8L);
        testVariable("i", VikariType.LONG, VikariType.LONG, 9L);
        testVariable("j", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10"));
        testVariable("k", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("11"));
        testVariable("l", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("12"));
        testVariable("m", VikariType.FLOAT, VikariType.FLOAT, 13.0F);
        testVariable("n", VikariType.FLOAT, VikariType.FLOAT, 14.0F);
        testVariable("o", VikariType.DOUBLE, VikariType.DOUBLE, 15.0D);
    }

    @Test
    @Order(17)
    public void testTreeWalkInterpreter_VariableDeclaration_NumericDowncasts_TruncatedValues_Overflow() {
        String sourceString = """
                a:Integer << 3000000000L
                b:Integer << 3000000000B
                c:Integer << 3000000000.0F
                d:Integer << 3000000000.0D
                e:Integer << 3000000000.0B
                f:Long << 10000000000000000000B
                g:Long << 10000000000000000000.0F
                h:Long << 10000000000000000000.0D
                i:Long << 10000000000000000000.0B
                j:BigInteger << 10
                k:BigInteger << 3000000000L
                l:BigInteger << 10000000000000000000B
                m:BigInteger << 10000000000000000000.0F
                n:BigInteger << 10000000000000000000.0D
                o:BigInteger << 10000000000000000000.0B
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, -1294967296);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, -1294967296);
        testVariable("c", VikariType.INTEGER, VikariType.INTEGER, -1294967296);
        testVariable("d", VikariType.INTEGER, VikariType.INTEGER, -1294967296);
        testVariable("e", VikariType.INTEGER, VikariType.INTEGER, -1294967296);
        testVariable("f", VikariType.LONG, VikariType.LONG, -8446744073709551616L);
        testVariable("g", VikariType.LONG, VikariType.LONG, -8446744093203103616L); // Floats are imprecise.
        testVariable("h", VikariType.LONG, VikariType.LONG, -8446744073709551616L);
        testVariable("i", VikariType.LONG, VikariType.LONG, -8446744073709551616L);
        testVariable("j", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10"));
        testVariable("k", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3000000000"));
        testVariable("l", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10000000000000000000"));
        testVariable("m", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10000000000000000000"));
        testVariable("n", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10000000000000000000"));
        testVariable("o", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10000000000000000000"));
    }

    @Test
    @Order(18)
    public void testTreeWalkInterpreter_VariableDeclaration_NumericDowncasts_TruncatedValues_Underflow() {
        String sourceString = """
                a:Integer << -3000000000L
                b:Integer << -3000000000B
                c:Integer << -3000000000.0F
                d:Integer << -3000000000.0D
                e:Integer << -3000000000.0B
                f:Long << -10000000000000000000B
                g:Long << -10000000000000000000.0F
                h:Long << -10000000000000000000.0D
                i:Long << -10000000000000000000.0B
                j:BigInteger << -10
                k:BigInteger << -3000000000L
                l:BigInteger << -10000000000000000000B
                m:BigInteger << -10000000000000000000.0F
                n:BigInteger << -10000000000000000000.0D
                o:BigInteger << -10000000000000000000.0B
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, 1294967296);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 1294967296);
        testVariable("c", VikariType.INTEGER, VikariType.INTEGER, 1294967296);
        testVariable("d", VikariType.INTEGER, VikariType.INTEGER, 1294967296);
        testVariable("e", VikariType.INTEGER, VikariType.INTEGER, 1294967296);
        testVariable("f", VikariType.LONG, VikariType.LONG, 8446744073709551616L);
        testVariable("g", VikariType.LONG, VikariType.LONG, 8446744093203103616L); // Floats are imprecise.
        testVariable("h", VikariType.LONG, VikariType.LONG, 8446744073709551616L);
        testVariable("i", VikariType.LONG, VikariType.LONG, 8446744073709551616L);
        testVariable("j", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("-10"));
        testVariable("k", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("-3000000000"));
        testVariable("l", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("-10000000000000000000"));
        testVariable("m", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("-10000000000000000000"));
        testVariable("n", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("-10000000000000000000"));
        testVariable("o", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("-10000000000000000000"));
    }

    @Test
    @Order(19)
    public void testTreeWalkInterpreter_VariableDeclaration_AssignmentToParentTypes() {
        String sourceString = """
                a << 1
                b:AtonementCrystal << 2
                c:Value << 3
                d:Number << 4
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("c", VikariType.VALUE, VikariType.INTEGER, 3);
        testVariable("d", VikariType.NUMBER, VikariType.INTEGER, 4);
    }

    @Test
    @Order(20)
    public void testTreeWalkInterpreter_VariableDeclaration_Booleans() {
        String sourceString = """
                a << true
                b << false
                c:Boolean
                d:Boolean << true
                e:Boolean << false
                f:AtonementCrystal << true
                g:AtonementCrystal << false
                h:Value << true
                i:Value << false
                """;

        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("c", VikariType.BOOLEAN);
        testVariable("d", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("e", VikariType.BOOLEAN, VikariType.BOOLEAN, false);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("g", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("h", VikariType.VALUE, VikariType.BOOLEAN, true);
        testVariable("i", VikariType.VALUE, VikariType.BOOLEAN, false);
    }

    @Test
    @Order(21)
    public void testTreeWalkInterpreter_VariableDeclaration_Booleans_FromVariable() {
        String sourceString = """
                foo:Boolean << true
                bar:Boolean << false

                a << foo
                b << bar
                c:Boolean
                d:Boolean << foo
                e:Boolean << bar
                f:AtonementCrystal << foo
                g:AtonementCrystal << bar
                h:Value << true
                i:Value << bar
                """;

        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("c", VikariType.BOOLEAN);
        testVariable("d", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("e", VikariType.BOOLEAN, VikariType.BOOLEAN, false);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("g", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("h", VikariType.VALUE, VikariType.BOOLEAN, true);
        testVariable("i", VikariType.VALUE, VikariType.BOOLEAN, false);
    }

    @Test
    @Order(22)
    public void testTreeWalkInterpreter_VariableDeclaration_Nulls_LengthZero() {
        String sourceString = """
                a
                b << null
                c << _[0]_
                d:Integer
                e:Integer << null
                f:Integer << __[0]__
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("a", VikariType.ATONEMENT_CRYSTAL, 0);
        testNullVariable("b", VikariType.ATONEMENT_CRYSTAL, 0);
        testNullVariable("c", VikariType.ATONEMENT_CRYSTAL, 0);
        testNullVariable("d", VikariType.INTEGER, 0);
        testNullVariable("e", VikariType.INTEGER, 0);
        testNullVariable("f", VikariType.INTEGER, 0);
    }

    @Test
    @Order(23)
    public void testTreeWalkInterpreter_VariableDeclaration_Nulls_PositiveLength() {
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
        lexParseAndInterpret(sourceString);

        testNullVariable("a", VikariType.ATONEMENT_CRYSTAL, 1);
        testNullVariable("b", VikariType.ATONEMENT_CRYSTAL, 2);
        testNullVariable("c", VikariType.ATONEMENT_CRYSTAL, 3);
        testNullVariable("d", VikariType.ATONEMENT_CRYSTAL, 4);
        testNullVariable("e", VikariType.ATONEMENT_CRYSTAL, 5);
        testNullVariable("f", VikariType.ATONEMENT_CRYSTAL, 1);
        testNullVariable("g", VikariType.ATONEMENT_CRYSTAL, 2);
        testNullVariable("h", VikariType.ATONEMENT_CRYSTAL, 3);
        testNullVariable("i", VikariType.ATONEMENT_CRYSTAL, 4);
        testNullVariable("j", VikariType.ATONEMENT_CRYSTAL, 5);
        testNullVariable("k", VikariType.INTEGER, 1);
        testNullVariable("l", VikariType.INTEGER, 2);
        testNullVariable("m", VikariType.INTEGER, 3);
        testNullVariable("n", VikariType.INTEGER, 4);
        testNullVariable("o", VikariType.INTEGER, 5);
        testNullVariable("p", VikariType.INTEGER, 1);
        testNullVariable("q", VikariType.INTEGER, 2);
        testNullVariable("r", VikariType.INTEGER, 3);
        testNullVariable("s", VikariType.INTEGER, 4);
        testNullVariable("t", VikariType.INTEGER, 5);
    }

    @Test
    @Order(23)
    public void testTreeWalkInterpreter_VariableDeclaration_Nulls_NegativeLength() {
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
        lexParseAndInterpret(sourceString);

        testNullVariable("a", VikariType.ATONEMENT_CRYSTAL, -1);
        testNullVariable("b", VikariType.ATONEMENT_CRYSTAL, -2);
        testNullVariable("c", VikariType.ATONEMENT_CRYSTAL, -3);
        testNullVariable("d", VikariType.ATONEMENT_CRYSTAL, -4);
        testNullVariable("e", VikariType.ATONEMENT_CRYSTAL, -5);
        testNullVariable("f", VikariType.INTEGER, -1);
        testNullVariable("g", VikariType.INTEGER, -2);
        testNullVariable("h", VikariType.INTEGER, -3);
        testNullVariable("i", VikariType.INTEGER, -4);
        testNullVariable("j", VikariType.INTEGER, -5);
    }

    @Test
    @Order(24)
    public void testTreeWalkInterpreter_VariableDeclaration_Nulls_NullLiteralExpression_WithVariable() {
        String sourceString = """
                int:Integer << 2
                foo << __[int]__
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("foo", VikariType.ATONEMENT_CRYSTAL, 2);
    }

    @Test
    @Order(25)
    public void testTreeWalkInterpreter_VariableDeclaration_Nulls_NullLiteralExpression_WithArithmeticExpression() {
        String sourceString = """
                foo << __[5 + 7]__
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("foo", VikariType.ATONEMENT_CRYSTAL, 12);
    }
}
