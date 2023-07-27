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
public class TreeWalkInterpreterTest_AssignmentExpressions extends TreeWalkInterpreterTest_Base {

    // -------------------------------
    // Left assignment operator tests.
    // -------------------------------

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_LeftAssignment_Basic() {
        lexParseAndInterpret("foo,foo << 2");
        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_LeftAssignment_WithTypeLabel() {
        String sourceString = "foo:Integer, foo << 3";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 3);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_LeftAssignment_WithTypeLabel_ReAssignment() {
        String sourceString = "foo:Integer << 2, foo << 4";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 4);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_LeftAssignment_MultipleVariables_SingleLine() {
        String sourceString = "foo, bar, baz, foo << 1, bar << 2, baz << 3";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 3);
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_LeftAssignment_MultipleVariables_MultipleLines() {
        String sourceString = """
                foo
                bar
                baz
                foo << 1
                bar << 2
                baz << 3
                """;
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 3);
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_LeftAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
        String sourceString = "foo:Integer, bar:Integer, baz:Integer, foo << 1, bar << 2, baz << 3";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 3);
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_LeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
        String sourceString = """
                foo:Integer
                bar:Integer
                baz:Integer
                foo << 1
                bar << 2
                baz << 3
                """;
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 3);
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_LeftAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = """
                foo:Integer << 1
                bar:Integer << 2
                baz:Integer << 3
                foo << 4
                bar << 5
                baz << 6
                """;
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 4);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 5);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 6);
    }

    @Test
    @Order(9)
    public void testTreeWalkInterpreter_LeftAssignment_AllNumericTypes() {
        String sourceString = """
                a, b, c, d, e, f
                a << 1
                b << 2L
                c << 3B
                d << 4.0F
                e << 5.0D
                f << 6.0B
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 2L);
        testVariable("c", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.ATONEMENT_CRYSTAL, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.ATONEMENT_CRYSTAL, VikariType.DOUBLE, 5.0D);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_DECIMAL, new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(10)
    public void testTreeWalkInterpreter_LeftAssignment_AllNumericTypes_WithTypeLabels() {
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
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("b", VikariType.LONG, VikariType.LONG, 2L);
        testVariable("c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.DOUBLE, VikariType.DOUBLE, 5.0D);
        testVariable("f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(11)
    public void testTreeWalkInterpreter_LeftAssignment_NumericUpcasts() {
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
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("long1", VikariType.LONG, VikariType.LONG, 1L);
        testVariable("bigInteger1", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("2"));
        testVariable("bigInteger2", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("float1", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("float2", VikariType.FLOAT, VikariType.FLOAT, 5.0F);
        testVariable("float3", VikariType.FLOAT, VikariType.FLOAT, 6.0F);
        testVariable("double1", VikariType.DOUBLE, VikariType.DOUBLE, 7.0D);
        testVariable("double2", VikariType.DOUBLE, VikariType.DOUBLE, 8.0D);
        testVariable("double3", VikariType.DOUBLE, VikariType.DOUBLE, 9.0D);
        testVariable("double4", VikariType.DOUBLE, VikariType.DOUBLE, 10.0D);
        testVariable("bigDecimal1", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("11", mathContext));
        testVariable("bigDecimal2", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("12", mathContext));
        testVariable("bigDecimal3", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("13", mathContext));
        testVariable("bigDecimal4", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("14.0", mathContext));
        testVariable("bigDecimal5", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(12)
    public void testTreeWalkInterpreter_LeftAssignment_NumericDowncasts() {
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
        lexParseAndInterpret(sourceString);

        testVariable("integer1", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("integer2", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("integer3", VikariType.INTEGER, VikariType.INTEGER, 3);
        testVariable("integer4", VikariType.INTEGER, VikariType.INTEGER, 4);
        testVariable("integer5", VikariType.INTEGER, VikariType.INTEGER, 5);
        testVariable("long1", VikariType.LONG, VikariType.LONG, 6L);
        testVariable("long2", VikariType.LONG, VikariType.LONG, 7L);
        testVariable("long3", VikariType.LONG, VikariType.LONG, 8L);
        testVariable("long4", VikariType.LONG, VikariType.LONG, 9L);
        testVariable("bigInteger1", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10"));
        testVariable("bigInteger2", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("11"));
        testVariable("bigInteger3", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("12"));
        testVariable("float1", VikariType.FLOAT, VikariType.FLOAT, 13.0F);
        testVariable("float2", VikariType.FLOAT, VikariType.FLOAT, 14.0F);
        testVariable("double1", VikariType.DOUBLE, VikariType.DOUBLE, 15.0D);
    }

    @Test
    @Order(13)
    public void testTreeWalkInterpreter_LeftAssignment_NumericDowncasts_TruncatedValues_Overflow() {
        String sourceString = """
                a:Integer
                b:Integer
                c:Integer
                d:Integer
                e:Integer
                f:Long
                g:Long
                h:Long
                i:Long
                j:BigInteger
                k:BigInteger
                l:BigInteger
                m:BigInteger
                n:BigInteger
                o:BigInteger

                a << 3000000000L
                b << 3000000000B
                c << 3000000000.0F
                d << 3000000000.0D
                e << 3000000000.0B
                f << 10000000000000000000B
                g << 10000000000000000000.0F
                h << 10000000000000000000.0D
                i << 10000000000000000000.0B
                j << 10
                k << 3000000000L
                l << 10000000000000000000B
                m << 10000000000000000000.0F
                n << 10000000000000000000.0D
                o << 10000000000000000000.0B
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
    @Order(14)
    public void testTreeWalkInterpreter_LeftAssignment_NumericDowncasts_TruncatedValues_Underflow() {
        String sourceString = """
                a:Integer
                b:Integer
                c:Integer
                d:Integer
                e:Integer
                f:Long
                g:Long
                h:Long
                i:Long
                j:BigInteger
                k:BigInteger
                l:BigInteger
                m:BigInteger
                n:BigInteger
                o:BigInteger

                a << -3000000000L
                b << -3000000000B
                c << -3000000000.0F
                d << -3000000000.0D
                e << -3000000000.0B
                f << -10000000000000000000B
                g << -10000000000000000000.0F
                h << -10000000000000000000.0D
                i << -10000000000000000000.0B
                j << -10
                k << -3000000000L
                l << -10000000000000000000B
                m << -10000000000000000000.0F
                n << -10000000000000000000.0D
                o << -10000000000000000000.0B
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
    @Order(15)
    public void testTreeWalkInterpreter_LeftAssignment_AssignmentToParentTypes() {
        String sourceString = """
                a, b:AtonementCrystal, c:Value, d:Number
                a << 1
                b << 2
                c << 3
                d << 4
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("c", VikariType.VALUE, VikariType.INTEGER, 3);
        testVariable("d", VikariType.NUMBER, VikariType.INTEGER, 4);
    }

    // --------------------------------
    // Right assignment operator tests.
    // --------------------------------

    @Test
    @Order(16)
    public void testTreeWalkInterpreter_RightAssignment_Basic() {
        lexParseAndInterpret("foo,2 >> foo");
        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);

    }

    @Test
    @Order(17)
    public void testTreeWalkInterpreter_RightAssignment_WithTypeLabel() {
        String sourceString = "foo:Integer, 3 >> foo";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 3);
    }

    @Test
    @Order(18)
    public void testTreeWalkInterpreter_RightAssignment_WithTypeLabel_ReAssignment() {
        String sourceString = "foo:Integer << 2, 4 >> foo";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 4);
    }

    @Test
    @Order(19)
    public void testTreeWalkInterpreter_RightAssignment_MultipleVariables_SingleLine() {
        String sourceString = "foo, bar, baz, 1 >> foo, 2 >> bar, 3 >> baz";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 3);
    }

    @Test
    @Order(20)
    public void testTreeWalkInterpreter_RightAssignment_MultipleVariables_MultipleLines() {
        String sourceString = """
                foo
                bar
                baz
                1 >> foo
                2 >> bar
                3 >> baz
                """;
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 3);
    }

    @Test
    @Order(21)
    public void testTreeWalkInterpreter_RightAssignment_MultipleVariables_SingleLine_WithTypeLabels() {
        String sourceString = "foo:Integer, bar:Integer, baz:Integer, 1 >> foo, 2 >> bar, 3 >> baz";
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 3);
    }

    @Test
    @Order(22)
    public void testTreeWalkInterpreter_RightAssignment_MultipleVariables_MultipleLines_WithTypeLabels() {
        String sourceString = """
                foo:Integer
                bar:Integer
                baz:Integer
                1 >> foo
                2 >> bar
                3 >> baz
                """;
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 3);
    }

    @Test
    @Order(23)
    public void testTreeWalkInterpreter_RightAssignment_MultipleVariables_MultipleLines_WithTypeLabels_AndInitializerExpressions() {
        String sourceString = """
                foo:Integer << 1
                bar:Integer << 2
                baz:Integer << 3
                4 >> foo
                5 >> bar
                6 >> baz
                """;
        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.INTEGER, 4);
        testVariable("bar", VikariType.INTEGER, VikariType.INTEGER, 5);
        testVariable("baz", VikariType.INTEGER, VikariType.INTEGER, 6);
    }

    @Test
    @Order(24)
    public void testTreeWalkInterpreter_RightAssignment_AllNumericTypes() {
        String sourceString = """
                a, b, c, d, e, f
                1 >> a
                2L >> b
                3B >> c
                4.0F >> d
                5.0D >> e
                6.0B >> f
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 2L);
        testVariable("c", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.ATONEMENT_CRYSTAL, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.ATONEMENT_CRYSTAL, VikariType.DOUBLE, 5.0D);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BIG_DECIMAL, new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(25)
    public void testTreeWalkInterpreter_RightAssignment_AllNumericTypes_WithTypeLabels() {
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
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("b", VikariType.LONG, VikariType.LONG, 2L);
        testVariable("c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.DOUBLE, VikariType.DOUBLE, 5.0D);
        testVariable("f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("6.0", Arithmetic.getMathContext()));
    }

    @Test
    @Order(26)
    public void testTreeWalkInterpreter_RightAssignment_NumericUpcasts() {
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
        lexParseAndInterpret(sourceString);

        MathContext mathContext = Arithmetic.getMathContext();

        testVariable("long1", VikariType.LONG, VikariType.LONG, 1L);
        testVariable("bigInteger1", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("2"));
        testVariable("bigInteger2", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("float1", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("float2", VikariType.FLOAT, VikariType.FLOAT, 5.0F);
        testVariable("float3", VikariType.FLOAT, VikariType.FLOAT, 6.0F);
        testVariable("double1", VikariType.DOUBLE, VikariType.DOUBLE, 7.0D);
        testVariable("double2", VikariType.DOUBLE, VikariType.DOUBLE, 8.0D);
        testVariable("double3", VikariType.DOUBLE, VikariType.DOUBLE, 9.0D);
        testVariable("double4", VikariType.DOUBLE, VikariType.DOUBLE, 10.0D);
        testVariable("bigDecimal1", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("11", mathContext));
        testVariable("bigDecimal2", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("12", mathContext));
        testVariable("bigDecimal3", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("13", mathContext));
        testVariable("bigDecimal4", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("14.0", mathContext));
        testVariable("bigDecimal5", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(27)
    public void testTreeWalkInterpreter_RightAssignment_NumericDowncasts() {
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
        lexParseAndInterpret(sourceString);

        testVariable("integer1", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("integer2", VikariType.INTEGER, VikariType.INTEGER, 2);
        testVariable("integer3", VikariType.INTEGER, VikariType.INTEGER, 3);
        testVariable("integer4", VikariType.INTEGER, VikariType.INTEGER, 4);
        testVariable("integer5", VikariType.INTEGER, VikariType.INTEGER, 5);
        testVariable("long1", VikariType.LONG, VikariType.LONG, 6L);
        testVariable("long2", VikariType.LONG, VikariType.LONG, 7L);
        testVariable("long3", VikariType.LONG, VikariType.LONG, 8L);
        testVariable("long4", VikariType.LONG, VikariType.LONG, 9L);
        testVariable("bigInteger1", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("10"));
        testVariable("bigInteger2", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("11"));
        testVariable("bigInteger3", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("12"));
        testVariable("float1", VikariType.FLOAT, VikariType.FLOAT, 13.0F);
        testVariable("float2", VikariType.FLOAT, VikariType.FLOAT, 14.0F);
        testVariable("double1", VikariType.DOUBLE, VikariType.DOUBLE, 15.0D);
    }

    @Test
    @Order(28)
    public void testTreeWalkInterpreter_RightAssignment_NumericDowncasts_TruncatedValues_Overflow() {
        String sourceString = """
                a:Integer
                b:Integer
                c:Integer
                d:Integer
                e:Integer
                f:Long
                g:Long
                h:Long
                i:Long
                j:BigInteger
                k:BigInteger
                l:BigInteger
                m:BigInteger
                n:BigInteger
                o:BigInteger

                3000000000L >> a
                3000000000B >> b
                3000000000.0F >> c
                3000000000.0D >> d
                3000000000.0B >> e
                10000000000000000000B >> f
                10000000000000000000.0F >> g
                10000000000000000000.0D >> h
                10000000000000000000.0B >> i
                10 >> j
                3000000000L >> k
                10000000000000000000B >> l
                10000000000000000000.0F >> m
                10000000000000000000.0D >> n
                10000000000000000000.0B >> o
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
    @Order(29)
    public void testTreeWalkInterpreter_RightAssignment_NumericDowncasts_TruncatedValues_Underflow() {
        String sourceString = """
                a:Integer
                b:Integer
                c:Integer
                d:Integer
                e:Integer
                f:Long
                g:Long
                h:Long
                i:Long
                j:BigInteger
                k:BigInteger
                l:BigInteger
                m:BigInteger
                n:BigInteger
                o:BigInteger

                -3000000000L >> a
                -3000000000B >> b
                -3000000000.0F >> c
                -3000000000.0D >> d
                -3000000000.0B >> e
                -10000000000000000000B >> f
                -10000000000000000000.0F >> g
                -10000000000000000000.0D >> h
                -10000000000000000000.0B >> i
                -10 >> j
                -3000000000L >> k
                -10000000000000000000B >> l
                -10000000000000000000.0F >> m
                -10000000000000000000.0D >> n
                -10000000000000000000.0B >> o
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
    @Order(30)
    public void testTreeWalkInterpreter_RightAssignment_AssignmentToParentTypes() {
        String sourceString = """
                a, b:AtonementCrystal, c:Value, d:Number
                1 >> a
                2 >> b
                3 >> c
                4 >> d
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 1);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, 2);
        testVariable("c", VikariType.VALUE, VikariType.INTEGER, 3);
        testVariable("d", VikariType.NUMBER, VikariType.INTEGER, 4);
    }

    @Test
    @Order(31)
    public void testTreeWalkInterpreter_LeftAssignment_Booleans() {
        String sourceString = """
                a
                b
                c:Boolean
                d:Boolean
                e:AtonementCrystal
                f:AtonementCrystal
                g:Value
                h:Value

                a << true
                b << false
                c << true
                d << false
                e << true
                f << false
                g << true
                h << false
                """;

        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("c", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("d", VikariType.BOOLEAN, VikariType.BOOLEAN, false);
        testVariable("e", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("g", VikariType.VALUE, VikariType.BOOLEAN, true);
        testVariable("h", VikariType.VALUE, VikariType.BOOLEAN, false);
    }

    @Test
    @Order(32)
    public void testTreeWalkInterpreter_RightAssignment_Booleans() {
        String sourceString = """
                a
                b
                c:Boolean
                d:Boolean
                e:AtonementCrystal
                f:AtonementCrystal
                g:Value
                h:Value

                true >> a
                false >> b
                true >> c
                false >> d
                true >> e
                false >> f
                true >> g
                false >> h
                """;

        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("c", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("d", VikariType.BOOLEAN, VikariType.BOOLEAN, false);
        testVariable("e", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("g", VikariType.VALUE, VikariType.BOOLEAN, true);
        testVariable("h", VikariType.VALUE, VikariType.BOOLEAN, false);
    }

    @Test
    @Order(33)
    public void testTreeWalkInterpreter_LeftAssignment_FromVariable() {
        String sourceString = """
                foo:Boolean << true
                bar:Boolean << false

                a
                b
                c:Boolean
                d:Boolean
                e:AtonementCrystal
                f:AtonementCrystal
                g:Value
                h:Value

                a << foo
                b << bar
                c << foo
                d << bar
                e << foo
                f << bar
                g << foo
                h << bar
                """;

        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("bar", VikariType.BOOLEAN, VikariType.BOOLEAN, false);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("c", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("d", VikariType.BOOLEAN, VikariType.BOOLEAN, false);
        testVariable("e", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("g", VikariType.VALUE, VikariType.BOOLEAN, true);
        testVariable("h", VikariType.VALUE, VikariType.BOOLEAN, false);
    }

    @Test
    @Order(34)
    public void testTreeWalkInterpreter_RightAssignment_FromVariable() {
        String sourceString = """
                foo:Boolean << true
                bar:Boolean << false

                a
                b
                c:Boolean
                d:Boolean
                e:AtonementCrystal
                f:AtonementCrystal
                g:Value
                h:Value

                foo >> a
                bar >> b
                foo >> c
                bar >> d
                foo >> e
                bar >> f
                foo >> g
                bar >> h
                """;

        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("bar", VikariType.BOOLEAN, VikariType.BOOLEAN, false);

        testVariable("a", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("b", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("c", VikariType.BOOLEAN, VikariType.BOOLEAN, true);
        testVariable("d", VikariType.BOOLEAN, VikariType.BOOLEAN, false);
        testVariable("e", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, true);
        testVariable("f", VikariType.ATONEMENT_CRYSTAL, VikariType.BOOLEAN, false);
        testVariable("g", VikariType.VALUE, VikariType.BOOLEAN, true);
        testVariable("h", VikariType.VALUE, VikariType.BOOLEAN, false);
    }

    @Test
    @Order(35)
    public void testTreeWalkInterpreter_LeftAssignment_Nulls_LengthZero() {
        String sourceString = """
                a, b
                a << null, b << _[0]_

                c:Integer, d:Integer
                c << null, d << __[0]__
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("a", VikariType.ATONEMENT_CRYSTAL, 0);
        testNullVariable("b", VikariType.ATONEMENT_CRYSTAL, 0);
        testNullVariable("c", VikariType.INTEGER, 0);
        testNullVariable("d", VikariType.INTEGER, 0);
    }

    @Test
    @Order(36)
    public void testTreeWalkInterpreter_LeftAssignment_Nulls_PositiveLength() {
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
    @Order(37)
    public void testTreeWalkInterpreter_LeftAssignment_Nulls_NegativeLength() {
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
    @Order(38)
    public void testTreeWalkInterpreter_LeftAssignment_Nulls_NullLiteralExpression_WithVariable() {
        String sourceString = """
                int:Integer << 2, foo
                foo << __[int]__
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("foo", VikariType.ATONEMENT_CRYSTAL, 2);
    }

    @Test
    @Order(39)
    public void testTreeWalkInterpreter_LeftAssignment_Nulls_NullLiteralExpression_WithArithmeticExpression() {
        String sourceString = """
                foo, foo << __[5 + 7]__
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("foo", VikariType.ATONEMENT_CRYSTAL, 12);
    }

    @Test
    @Order(40)
    public void testTreeWalkInterpreter_LeftAssignment_Nulls_FromVariable() {
        String sourceString = """
                foo:Integer
                bar:Integer << 2
                bar << foo
                """;

        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.NULL, 0);
        testVariable("bar", VikariType.INTEGER, VikariType.NULL, 0);
    }

    @Test
    @Order(41)
    public void testTreeWalkInterpreter_RightAssignment_Nulls_LengthZero() {
        String sourceString = """
                a, b
                null >> a, _[0]_ >> b

                c:Integer, d:Integer
                null >> c, __[0]__ >> d
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("a", VikariType.ATONEMENT_CRYSTAL, 0);
        testNullVariable("b", VikariType.ATONEMENT_CRYSTAL, 0);
        testNullVariable("c", VikariType.INTEGER, 0);
        testNullVariable("d", VikariType.INTEGER, 0);
    }

    @Test
    @Order(42)
    public void testTreeWalkInterpreter_RightAssignment_Nulls_PositiveLength() {
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
    @Order(43)
    public void testTreeWalkInterpreter_RightAssignment_Nulls_NegativeLength() {
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
    @Order(44)
    public void testTreeWalkInterpreter_RightAssignment_Nulls_NullLiteralExpression_WithVariable() {
        String sourceString = """
                int:Integer << 2, foo
                __[int]__ >> foo
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("foo", VikariType.ATONEMENT_CRYSTAL, 2);
    }

    @Test
    @Order(45)
    public void testTreeWalkInterpreter_RightAssignment_Nulls_NullLiteralExpression_WithArithmeticExpression() {
        String sourceString = """
                foo, __[5 + 7]__ >> foo
                """;
        lexParseAndInterpret(sourceString);

        testNullVariable("foo", VikariType.ATONEMENT_CRYSTAL, 12);
    }

    @Test
    @Order(46)
    public void testTreeWalkInterpreter_RightAssignment_Nulls_FromVariable() {
        String sourceString = """
                foo:Integer
                bar:Integer << 2
                foo >> bar
                """;

        lexParseAndInterpret(sourceString);

        testVariable("foo", VikariType.INTEGER, VikariType.NULL, 0);
        testVariable("bar", VikariType.INTEGER, VikariType.NULL, 0);
    }
}
