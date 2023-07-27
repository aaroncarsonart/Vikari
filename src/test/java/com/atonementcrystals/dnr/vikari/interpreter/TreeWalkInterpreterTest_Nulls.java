package com.atonementcrystals.dnr.vikari.interpreter;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_Nulls extends TreeWalkInterpreterPrintTest_Base {

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_Expression_Nulls_NullKeyword() {
        lexParseAndInterpret(":null");
        testOutput("null");
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_Expression_Nulls_SwordLiterals() {
        lexParseAndInterpret(":_");
        testOutput("Null::{length=1}");

        lexParseAndInterpret(":__");
        testOutput("Null::{length=2}");

        lexParseAndInterpret(":___");
        testOutput("Null::{length=3}");

        lexParseAndInterpret(":____");
        testOutput("Null::{length=4}");
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_Expression_Nulls_NullLiteralExpressions() {
        lexParseAndInterpret(":_[0]_");
        testOutput("null");

        lexParseAndInterpret(":_[-1]_");
        testOutput("Null::{length=-1}");

        lexParseAndInterpret(":_[1]_");
        testOutput("Null::{length=1}");

        lexParseAndInterpret(":_[5]_");
        testOutput("Null::{length=5}");

        lexParseAndInterpret(":_[-21]_");
        testOutput("Null::{length=-21}");
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_Expression_Nulls_NullLiteralExpressions_LongerSwords() {
        lexParseAndInterpret(":__[0]_");
        testOutput("null");

        lexParseAndInterpret(":_[0]____");
        testOutput("null");

        lexParseAndInterpret(":__[0]__");
        testOutput("null");

        lexParseAndInterpret(":___[0]___");
        testOutput("null");
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_Expression_Nulls_NullLiteralExpressions_AllNumericTypes() {
        lexParseAndInterpret(":__[5]__");
        testOutput("Null::{length=5}");

        lexParseAndInterpret(":__[5L]__");
        testOutput("Null::{length=5}");

        lexParseAndInterpret(":__[5B]__");
        testOutput("Null::{length=5}");

        lexParseAndInterpret(":__[5.0F]__");
        testOutput("Null::{length=5}");

        lexParseAndInterpret(":__[5.0D]__");
        testOutput("Null::{length=5}");

        lexParseAndInterpret(":__[5.0B]__");
        testOutput("Null::{length=5}");
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_Expression_Nulls_NullLiteralExpressions_AllNumericTypes_Overflow() {
        lexParseAndInterpret("foo << __[3000000000L]__, :foo");
        testOutput("Null::{length=-1294967296}");

        lexParseAndInterpret("foo << __[3000000000B]__, :foo");
        testOutput("Null::{length=-1294967296}");

        lexParseAndInterpret("foo << __[3000000000.0F]__, :foo");
        testOutput("Null::{length=-1294967296}");

        lexParseAndInterpret("foo << __[3000000000.0D]__, :foo");
        testOutput("Null::{length=-1294967296}");

        lexParseAndInterpret("foo << __[3000000000.0B]__, :foo");
        testOutput("Null::{length=-1294967296}");
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_Expression_Nulls_NullLiteralExpressions_DecimalTruncation() {
        lexParseAndInterpret("foo << __[3.14F]__, :foo");
        testOutput("Null::{length=3}");

        lexParseAndInterpret("foo << __[3.14D]__, :foo");
        testOutput("Null::{length=3}");

        lexParseAndInterpret("foo << __[3.14B]__, :foo");
        testOutput("Null::{length=3}");
    }
}
