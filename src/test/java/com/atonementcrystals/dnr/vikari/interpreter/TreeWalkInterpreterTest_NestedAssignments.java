package com.atonementcrystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_NestedAssignments extends TreeWalkInterpreterPrintTest_Base {

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_LeftAssignment_NestedAssignmentExpressions_Numbers() {
        String sourceString = """
                a, b, c
                a << b << c << 2
                :a:b:c:
                a << b << c << 3
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("222\n333\n");
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_LeftAssignment_NestedAssignmentExpressions_Numbers_WithArithmetic() {
        String sourceString = """
                a, b, c
                c << 1 + [b << 1 + [a << 1]]
                :a:b:c:
                c << 1 + [b << 1 + [a << 4]]
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("123\n456\n");
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_RightAssignment_NestedAssignmentExpressions_Numbers() {
        String sourceString = """
                a, b, c
                [[2 >> a] >> b] >> c
                :a:b:c:
                [[3 >> a] >> b] >> c
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("222\n333\n");
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_RightAssignment_NestedAssignmentExpressions_Numbers_WithArithmetic() {
        String sourceString = """
                a, b, c
                [[1 >> a] + 1 >> b] + 1 >> c
                :a:b:c:
                [[4 >> a] + 1 >> b] + 1 >> c
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("123\n456\n");
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_LeftAssignment_NestedAssignmentExpressions_Booleans() {
        String sourceString = """
                a, b, c
                a << b << c << true
                :a:b:c:
                a << b << c << false
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("truetruetrue\nfalsefalsefalse\n");
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_RightAssignment_NestedAssignmentExpressions_Booleans() {
        String sourceString = """
                a, b, c
                [[true >> a] >> b] >> c
                :a:b:c:
                [[false >> a] >> b] >> c
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("truetruetrue\nfalsefalsefalse\n");
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_LeftAssignment_NestedAssignmentExpressions_Nulls() {
        String sourceString = """
                a, b, c
                :a:b:c:
                a << b << c << __
                :a:b:c:
                a << b << c << __[-1]__
                :a:b:c:
                a << b << c << null
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("null".repeat(3) + "\n" +
                "Null::{length=2}".repeat(3) + "\n" +
                "Null::{length=-1}".repeat(3) + "\n" +
                "null".repeat(3) + "\n");
    }

    @Test
    @Order(8)
    public void testTreeWalkInterpreter_RightAssignment_NestedAssignmentExpressions_Nulls() {
        String sourceString = """
                a, b, c
                :a:b:c:
                [[__ >> a] >> b] >> c
                :a:b:c:
                [[__[-1]__ >> a] >> b] >> c
                :a:b:c:
                [[null >> a] >> b] >> c
                :a:b:c:
                """;
        lexParseAndInterpret(sourceString);
        testOutput("null".repeat(3) + "\n" +
                "Null::{length=2}".repeat(3) + "\n" +
                "Null::{length=-1}".repeat(3) + "\n" +
                "null".repeat(3) + "\n");
    }
}
