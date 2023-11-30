package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_VariableExpressions extends TreeWalkInterpreterTest_Base {

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

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 22L);
        testVariable("a", VikariType.NUMBER, VikariType.LONG, 29L);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 29);
        testVariable("c", VikariType.LONG, VikariType.LONG, 29L);
        testVariable("d", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("29"));
        testVariable("e", VikariType.FLOAT, VikariType.FLOAT, 29.0F);
        testVariable("f", VikariType.DOUBLE, VikariType.DOUBLE, 29.0D);
        testVariable("g", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("29"));
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

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 22L);
        testVariable("a", VikariType.NUMBER, VikariType.LONG, 29L);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 29);
        testVariable("c", VikariType.LONG, VikariType.LONG, 29L);
        testVariable("d", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("29"));
        testVariable("e", VikariType.FLOAT, VikariType.FLOAT, 29.0F);
        testVariable("f", VikariType.DOUBLE, VikariType.DOUBLE, 29.0D);
        testVariable("g", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("29"));
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

        testVariable("foo", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, 22L);
        testVariable("a", VikariType.NUMBER, VikariType.LONG, 29L);
        testVariable("b", VikariType.INTEGER, VikariType.INTEGER, 29);
        testVariable("c", VikariType.LONG, VikariType.LONG, 29L);
        testVariable("d", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("29"));
        testVariable("e", VikariType.FLOAT, VikariType.FLOAT, 29.0F);
        testVariable("f", VikariType.DOUBLE, VikariType.DOUBLE, 29.0D);
        testVariable("g", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("29"));
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

                foo:Number << a + b - c * d / e \\ f
                """;
        lexParseAndInterpret(sourceString);

        testVariable("a", VikariType.INTEGER, VikariType.INTEGER, 1);
        testVariable("b", VikariType.LONG, VikariType.LONG, 2L);
        testVariable("c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, new BigInteger("3"));
        testVariable("d", VikariType.FLOAT, VikariType.FLOAT, 4.0F);
        testVariable("e", VikariType.DOUBLE, VikariType.DOUBLE, 5.0D);
        testVariable("f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, new BigDecimal("6"));
        testVariable("foo", VikariType.NUMBER, VikariType.BIG_DECIMAL, new BigDecimal("0.5"));
    }
}
