package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static com.atonementcrystals.dnr.vikari.TestUtils.testCrystal;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.lexSingleStatement;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Comments {

    private void lexVariableDeclarationWithComment(String sourceString, int col1, int col2, int col3, int col4, int col5) {
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, col1));
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location(0, col2));
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location(0, col3));
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location(0, col4));
        testCrystal(statement.get(4), IntegerCrystal.class, "2", location(0, col5));
    }

    private void lexVariableDeclarationWithMultiLineComment(String sourceString, CoordinatePair location1,
                                                            CoordinatePair location2, CoordinatePair location3,
                                                            CoordinatePair location4, CoordinatePair location5) {
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location1);
        testCrystal(statement.get(1), TypeLabelOperatorCrystal.class, ":", location2);
        testCrystal(statement.get(2), TypeReferenceCrystal.class, "Integer", location3);
        testCrystal(statement.get(3), LeftAssignmentOperatorCrystal.class, "<<", location4);
        testCrystal(statement.get(4), IntegerCrystal.class, "2", location5);
    }

    @Test
    @Order(1)
    public void testLexer_Crystals_SingleLineComment_BetweenTokensOfAssignmentStatement() {
        lexVariableDeclarationWithComment("~:This is a comment.:~ foo:Integer << 2", 23, 26, 27, 35, 38);
        lexVariableDeclarationWithComment("foo~:This is a comment.:~:Integer << 2", 0, 25, 26, 34, 37);
        lexVariableDeclarationWithComment("foo:~:This is a comment.:~Integer << 2", 0, 3, 26, 34, 37);
        lexVariableDeclarationWithComment("foo:Integer ~:This is a comment.:~ << 2", 0, 3, 4, 35, 38);
        lexVariableDeclarationWithComment("foo:Integer << ~:This is a comment.:~ 2", 0, 3, 4, 12, 38);
        lexVariableDeclarationWithComment("foo:Integer << 2 ~:This is a comment.:~", 0, 3, 4, 12, 15);
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_MultiLineComment_TwoLineCommentBetweenTokensOfAssignmentStatement() {
        lexVariableDeclarationWithMultiLineComment("~:This is a\nmulti-line comment.:~ foo:Integer << 2",
                location(1, 22), location(1, 25), location(1, 26), location(1, 34), location(1, 37));

        lexVariableDeclarationWithMultiLineComment("foo~:This is a\nmulti-line comment.:~:Integer << 2",
                location(0, 0), location(1, 21), location(1, 22), location(1, 30), location(1, 33));

        lexVariableDeclarationWithMultiLineComment("foo:~:This is a\nmulti-line comment.:~Integer << 2",
                location(0, 0), location(0, 3), location(1, 21), location(1, 29), location(1, 32));

        lexVariableDeclarationWithMultiLineComment("foo:Integer ~:This is a\nmulti-line comment.:~ << 2",
                location(0, 0), location(0, 3), location(0, 4), location(1, 22), location(1, 25));

        lexVariableDeclarationWithMultiLineComment("foo:Integer << ~:This is a\nmulti-line comment.:~ 2",
                location(0, 0), location(0, 3), location(0, 4), location(0, 12), location(1, 22));

        lexVariableDeclarationWithMultiLineComment("foo:Integer << 2 ~:This is a\nmulti-line comment.:~",
                location(0, 0), location(0, 3), location(0, 4), location(0, 12), location(0, 15));
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_MultiLineComment_ThreeLineCommentBetweenTokensOfAssignmentStatement() {
        lexVariableDeclarationWithMultiLineComment("~:This is a\nthree-line\ncomment.:~ foo:Integer << 2",
                location(2, 11), location(2, 14), location(2, 15), location(2, 23), location(2, 26));

        lexVariableDeclarationWithMultiLineComment("foo~:This is a\nthree-line\ncomment.:~:Integer << 2",
                location(0, 0), location(2, 10), location(2, 11), location(2, 19), location(2, 22));

        lexVariableDeclarationWithMultiLineComment("foo:~:This is a\nthree-line\ncomment.:~Integer << 2",
                location(0, 0), location(0, 3), location(2, 10), location(2, 18), location(2, 21));

        lexVariableDeclarationWithMultiLineComment("foo:Integer ~:This is a\nthree-line\ncomment.:~ << 2",
                location(0, 0), location(0, 3), location(0, 4), location(2, 11), location(2, 14));

        lexVariableDeclarationWithMultiLineComment("foo:Integer << ~:This is a\nthree-line\ncomment.:~ 2",
                location(0, 0), location(0, 3), location(0, 4), location(0, 12), location(2, 11));

        lexVariableDeclarationWithMultiLineComment("foo:Integer << 2 ~:This is a\nthree-line\ncomment.:~",
                location(0, 0), location(0, 3), location(0, 4), location(0, 12), location(0, 15));
    }
}
