package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.control.flow.ConditionalBranchCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.SwordCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.DotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.angelguard.LeftFeatherFallCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.angelguard.RightFeatherFallCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.EqualsOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalNotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.IndexOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.RangeOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RegionOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RegionSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.LeftSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RightSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.ListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RightParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.*;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * The negation operator - is sometimes collapsed with the following token when that
 * token is a numeric literal. This is based on what kind of token precedes the
 * negation operator. This test class exhaustively tests every scenario.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_NegationOperator {

    /**
     * Test that every TokenType either collapses the negation operator, or does not.
     */
    @Test
    @Order(1)
    public void testLexer_Crystals_NegationOperator_EnsureAllTokenTypesAreHandled() {
        // Manually specify all TokenTypes that don't collapse the negation operator.
        List<TokenType> nonCollapsingTokenTypes = List.of(
                TokenType.FUNCTION_CALL,
                TokenType.STATIC_FIELD_ACCESS,
                TokenType.ANNOTATION,
                TokenType.RIGHT_PARENTHESIS,
                TokenType.CATCH,
                TokenType.THROW,
                TokenType.DOT,
                TokenType.VARIABLE_ARGUMENTS_LIST,
                TokenType.REGION_OPERATOR,
                TokenType.LOOP,
                TokenType.CONDITIONAL_BRANCH,
                TokenType.INSTANCE_FIELD_ACCESS,
                TokenType.RIGHT_SQUARE_BRACKET,
                TokenType.SWORD,
                TokenType.RIGHT_CURLY_BRACKET,
                TokenType.RIGHT_FEATHER_FALL);

        // Build a map of the expected crystal counts for a statement of the form "token - 1".
        Map<TokenType, Integer> expectedTokenTypeCrystalCounts = new LinkedHashMap<>();
        for (TokenType tokenType : TokenType.values()) {
            if (Lexer.COLLAPSE_NEGATION_OPERATOR_CLASSES.contains(tokenType.getJavaType())) {
                expectedTokenTypeCrystalCounts.put(tokenType, 2);
            }
        }
        for (TokenType tokenType : nonCollapsingTokenTypes) {
            expectedTokenTypeCrystalCounts.put(tokenType, 3);
        }

        // List of TokenTypes to ignore.
        List<TokenType> duplicatesAndSpecialCases = List.of(
                TokenType.COMMENT_PREFIX_CRYSTAL,
                TokenType.COMMENT_SUFFIX_CRYSTAL,
                TokenType.BACKTICK,
                TokenType.CAPTURE_QUOTATION,
                TokenType.LINE_CONTINUATION,
                TokenType.MINIMIZED_LINE_CONTINUATION,
                TokenType.STATEMENT_SEPARATOR,
                TokenType.INSTANCE_SUPER,
                TokenType.STATIC_SUPER,
                TokenType.NEGATE,
                TokenType.RIGHT_ASSIGNMENT,
                TokenType.CONCATENATE,
                TokenType.PRINT_STATEMENT,
                TokenType.COLLECTION_LITERAL,
                TokenType.EXISTS,
                TokenType.CAST,
                TokenType.FUNCTION_PARAMETER_LIST_OPENING_BRACKET,
                TokenType.FUNCTION_PARAMETER_LIST_CLOSING_BRACKET,
                TokenType.FUNCTION_PARAMETER_LIST_ELEMENT_SEPARATOR,
                TokenType.FUNCTION_ARGUMENT_LIST_OPENING_BRACKET,
                TokenType.FUNCTION_ARGUMENT_LIST_CLOSING_BRACKET,
                TokenType.FUNCTION_ARGUMENT_LIST_ELEMENT_SEPARATOR,
                TokenType.LIST_LITERAL_OPENING_BRACKET,
                TokenType.LIST_LITERAL_CLOSING_BRACKET,
                TokenType.SET_LITERAL_OPENING_BRACKET,
                TokenType.SET_LITERAL_CLOSING_BRACKET,
                TokenType.ARRAY_LITERAL_OPENING_BRACKET,
                TokenType.ARRAY_LITERAL_CLOSING_BRACKET,
                TokenType.COLLECTION_LITERAL_ELEMENT_SEPARATOR,
                TokenType.ANNOTATION_OPENING_BRACKET,
                TokenType.ANNOTATION_CLOSING_BRACKET,
                TokenType.ANNOTATION_ELEMENT_SEPARATOR
        );

        // Get all TokenTypes, but ignore the duplicates and special cases.
        List<TokenType> tokenTypesToTest = Arrays.stream(TokenType.values())
                .filter(tokenType -> !duplicatesAndSpecialCases.contains(tokenType))
                .sorted()
                .toList();

        // Determine if any TokenTypes are not handled by this test method.
        List<String> unhandledTokenTypes = tokenTypesToTest.stream()
                .filter(tokenType -> !expectedTokenTypeCrystalCounts.containsKey(tokenType))
                .map(TokenType::getIdentifier)
                .sorted()
                .toList();

        // Detect if any new TokenTypes aren't handled by this unit test.
        if (!unhandledTokenTypes.isEmpty()) {
            fail("Unhandled TokenTypes in negation operator collapsion test: "  + unhandledTokenTypes);
        }

        // Test the statements for each operator.
        for (TokenType tokenType : tokenTypesToTest) {
            String tokenTypeIdentifier = tokenType.getIdentifier();
            String sourceString = tokenType.getIdentifier() + " - 1";
            int expectedCrystalCount = expectedTokenTypeCrystalCounts.get(tokenType);
            List<AtonementCrystal> statement = lexSingleStatement(sourceString, expectedCrystalCount);

            Class<? extends AtonementCrystal> expectedType = tokenType.getJavaType();
            testCrystal(statement.get(0), expectedType, tokenTypeIdentifier, location(0, 0));

            int locationOffset = tokenTypeIdentifier.length();

            // Negation operator collapsion case.
            if (expectedCrystalCount == 2) {
                testCrystal(statement.get(1), IntegerCrystal.class, "-1", location(0, locationOffset + 3));
            }

            // Non-negation operator collapsion case.
            else {
                testCrystal(statement.get(1), SubtractOperatorCrystal.class, "-", location(0, locationOffset + 1));
                testCrystal(statement.get(2), IntegerCrystal.class, "1", location(0, locationOffset + 3));
            }
        }
    }

    private void testUnaryOperatorExpression(String sourceString, TokenType unaryOperator) {
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 2);

        Class<? extends AtonementCrystal> operatorClass = unaryOperator.getJavaType();
        String operatorIdentifier = unaryOperator.getIdentifier();
        int operatorLength = operatorIdentifier.length();

        testCrystal(statement.get(0), operatorClass, operatorIdentifier, location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "-5", location(0, operatorLength + 2));

        testNegationOperatorLocation(statement.get(1), location(0, operatorLength + 1));
    }

    private void testBinaryOperatorExpression(String sourceString, TokenType binaryOperator) {
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 3);

        Class<? extends AtonementCrystal> operatorClass = binaryOperator.getJavaType();
        String operatorIdentifier = binaryOperator.getIdentifier();
        int operatorLength = operatorIdentifier.length();

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
        testCrystal(statement.get(1), operatorClass, operatorIdentifier, location(0, 4));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(0, 4 + operatorLength + 2));

        testNegationOperatorLocation(statement.get(2), location(0, 4 + operatorLength + 1));
    }

    private void testAssignmentOperatorExpression(String sourceString, TokenType assignmentOperator) {
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 3);

        Class<? extends AtonementCrystal> operatorClass = assignmentOperator.getJavaType();
        String operatorIdentifier = assignmentOperator.getIdentifier();
        int operatorLength = operatorIdentifier.length();

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
        testCrystal(statement.get(1), operatorClass, operatorIdentifier, location(0, 4));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(0, 4 + operatorLength + 2));

        testNegationOperatorLocation(statement.get(2), location(0, 4 + operatorLength + 1));
    }

    private void testLogicalOperatorExpression(String sourceString, TokenType assignmentOperator) {
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 5);

        Class<? extends AtonementCrystal> operatorClass = assignmentOperator.getJavaType();
        String operatorIdentifier = assignmentOperator.getIdentifier();
        int operatorLength = operatorIdentifier.length();

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
        testCrystal(statement.get(1), operatorClass, operatorIdentifier, location(0, 4));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(0, 4 + operatorLength + 2));
        testCrystal(statement.get(3), EqualsOperatorCrystal.class, "=", location(0, 4 + operatorLength + 4));
        testCrystal(statement.get(4), ReferenceCrystal.class, "bar", location(0, 4 + operatorLength + 6));

        testNegationOperatorLocation(statement.get(2), location(0, 4 + operatorLength + 1));
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_NegationOperator_Basic_NoSpace() {
        String sourceString = "-1\n-2L\n-3B\n-4.0F\n-5.0D\n-6.0B";

        List<List<AtonementCrystal>> statements = lex(sourceString, 6, crystalCounts(1, 1, 1, 1, 1, 1));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "-1", location(0, 1));
        testCrystal(statements.get(1).get(0), LongCrystal.class, "-2L", location(1, 1));
        testCrystal(statements.get(2).get(0), BigIntegerCrystal.class, "-3B", location(2, 1));
        testCrystal(statements.get(3).get(0), FloatCrystal.class, "-4.0F", location(3, 1));
        testCrystal(statements.get(4).get(0), DoubleCrystal.class, "-5.0D", location(4, 1));
        testCrystal(statements.get(5).get(0), BigDecimalCrystal.class, "-6.0B", location(5, 1));

        testNegationOperatorLocation(statements.get(0).get(0), location(0, 0));
        testNegationOperatorLocation(statements.get(1).get(0), location(1, 0));
        testNegationOperatorLocation(statements.get(2).get(0), location(2, 0));
        testNegationOperatorLocation(statements.get(3).get(0), location(3, 0));
        testNegationOperatorLocation(statements.get(4).get(0), location(4, 0));
        testNegationOperatorLocation(statements.get(5).get(0), location(5, 0));
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_NegationOperator_Basic_WithSpace() {
        String sourceString = "- 1\n- 2L\n- 3B\n- 4.0F\n- 5.0D\n- 6.0B";

        List<List<AtonementCrystal>> statements = lex(sourceString, 6, crystalCounts(1, 1, 1, 1, 1, 1));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "-1", location(0, 2));
        testCrystal(statements.get(1).get(0), LongCrystal.class, "-2L", location(1, 2));
        testCrystal(statements.get(2).get(0), BigIntegerCrystal.class, "-3B", location(2, 2));
        testCrystal(statements.get(3).get(0), FloatCrystal.class, "-4.0F", location(3, 2));
        testCrystal(statements.get(4).get(0), DoubleCrystal.class, "-5.0D", location(4, 2));
        testCrystal(statements.get(5).get(0), BigDecimalCrystal.class, "-6.0B", location(5, 2));

        testNegationOperatorLocation(statements.get(0).get(0), location(0, 0));
        testNegationOperatorLocation(statements.get(1).get(0), location(1, 0));
        testNegationOperatorLocation(statements.get(2).get(0), location(2, 0));
        testNegationOperatorLocation(statements.get(3).get(0), location(3, 0));
        testNegationOperatorLocation(statements.get(4).get(0), location(4, 0));
        testNegationOperatorLocation(statements.get(5).get(0), location(5, 0));
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_NegationOperator_AfterUnaryOperators() {
        testUnaryOperatorExpression("^^ -5", TokenType.RETURN);
        testUnaryOperatorExpression(">> -5", TokenType.CONTINUE);
        testUnaryOperatorExpression("vv -5", TokenType.BREAK);
        testUnaryOperatorExpression(": -5", TokenType.TYPE_LABEL);
        testUnaryOperatorExpression("& -5", TokenType.COPY_CONSTRUCTOR);
        testUnaryOperatorExpression("|| -5", TokenType.CATCH_ALL);
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_NegationOperator_AfterLeftSquareBracket() {
        List<AtonementCrystal> statement = lexSingleStatement("[-5]", 3);

        testCrystal(statement.get(0), LeftSquareBracketCrystal.class, "[", location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "-5", location(0, 2));
        testCrystal(statement.get(2), RightSquareBracketCrystal.class, "]", location(0, 3));

        testNegationOperatorLocation(statement.get(1), location(0, 1));

    }

    @Test
    @Order(6)
    public void testLexer_Crystals_NegationOperator_AfterStatementSeparator() {
        List<List<AtonementCrystal>> statements = lex("5,-5", 2, crystalCounts(1, 1));

        testCrystal(statements.get(0).get(0), IntegerCrystal.class, "5", location(0, 0));
        testCrystal(statements.get(1).get(0), IntegerCrystal.class, "-5", location(0, 3));

        testNegationOperatorLocation(statements.get(1).get(0), location(0, 2));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_NegationOperator_AfterRegionSeparator() {
        List<AtonementCrystal> statement = lexSingleStatement("?? [true] :: _; -5", 8);

        testCrystal(statement.get(0), ConditionalBranchCrystal.class, "??", location(0, 0));
        testCrystal(statement.get(1), LeftSquareBracketCrystal.class, "[", location(0, 3));
        testCrystal(statement.get(2), BooleanCrystal.class, "true", location(0, 4));
        testCrystal(statement.get(3), RightSquareBracketCrystal.class, "]", location(0, 8));
        testCrystal(statement.get(4), RegionOperatorCrystal.class, "::", location(0, 10));
        testCrystal(statement.get(5), SwordCrystal.class, "_", location(0, 13));
        testCrystal(statement.get(6), RegionSeparatorCrystal.class, ";", location(0, 14));
        testCrystal(statement.get(7), IntegerCrystal.class, "-5", location(0, 17));

        testNegationOperatorLocation(statement.get(7), location(0, 16));
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_NegationOperator_AfterLeftParenthesis() {
        List<AtonementCrystal> statement = lexSingleStatement("(-5)", 3);

        testCrystal(statement.get(0), LeftParenthesisCrystal.class, "(", location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "-5", location(0, 2));
        testCrystal(statement.get(2), RightParenthesisCrystal.class, ")", location(0, 3));

        testNegationOperatorLocation(statement.get(1), location(0, 1));
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_NegationOperator_AfterListElementSeparator() {
        List<AtonementCrystal> statement = lexSingleStatement("(5|-5)", 5);

        testCrystal(statement.get(0), LeftParenthesisCrystal.class, "(", location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "5", location(0, 1));
        testCrystal(statement.get(2), ListElementSeparatorCrystal.class, "|", location(0, 2));
        testCrystal(statement.get(3), IntegerCrystal.class, "-5", location(0, 4));
        testCrystal(statement.get(4), RightParenthesisCrystal.class, ")", location(0, 5));

        testNegationOperatorLocation(statement.get(3), location(0, 3));
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_NegationOperator_AfterRangeOperator() {
        List<AtonementCrystal> statement = lexSingleStatement("5..-5", 3);

        testCrystal(statement.get(0), IntegerCrystal.class, "5", location(0, 0));
        testCrystal(statement.get(1), RangeOperatorCrystal.class, "..", location(0, 1));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(0, 4));

        testNegationOperatorLocation(statement.get(2), location(0, 3));
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_NegationOperator_AfterIndexOperator() {
        List<AtonementCrystal> statement = lexSingleStatement("foo.$-5", 4);

        testCrystal(statement.get(0), ReferenceCrystal.class, "foo", location(0, 0));
        testCrystal(statement.get(1), DotOperatorCrystal.class, ".", location(0, 3));
        testCrystal(statement.get(2), IndexOperatorCrystal.class, "$", location(0, 4));
        testCrystal(statement.get(3), IntegerCrystal.class, "-5", location(0, 6));

        testNegationOperatorLocation(statement.get(3), location(0, 5));
    }

    @Test
    @Order(12)
    public void testLexer_Crystals_NegationOperator_AfterBinaryOperators() {
        testBinaryOperatorExpression("foo % -5", TokenType.MODULUS);
        testBinaryOperatorExpression("foo * -5", TokenType.MULTIPLY);
        testBinaryOperatorExpression("foo - -5", TokenType.SUBTRACT);
        testBinaryOperatorExpression("foo + -5", TokenType.ADD);
        testBinaryOperatorExpression("foo \\ -5", TokenType.RIGHT_DIVIDE);
        testBinaryOperatorExpression("foo / -5", TokenType.LEFT_DIVIDE);
        testBinaryOperatorExpression("foo <- -5", TokenType.ITERATION_ELEMENT);
        testBinaryOperatorExpression("foo ? -5", TokenType.INSTANCE_OF);
        // TODO: Update the Lexer and this test class to reflect new TokenTypes.
        testBinaryOperatorExpression("foo => -5", TokenType.KEY_VALUE_PAIR);
    }

    @Test
    @Order(13)
    public void testLexer_Crystals_NegationOperator_AfterAssignmentOperators() {
        testAssignmentOperatorExpression("foo << -5", TokenType.LEFT_ASSIGNMENT);
        testAssignmentOperatorExpression("foo +<< -5", TokenType.LEFT_ADD_ASSIGNMENT);
        testAssignmentOperatorExpression("foo -<< -5", TokenType.LEFT_SUBTRACT_ASSIGNMENT);
        testAssignmentOperatorExpression("foo /<< -5", TokenType.LEFT_DIVIDE_ASSIGNMENT);
        testAssignmentOperatorExpression("foo *<< -5", TokenType.LEFT_MULTIPLY_ASSIGNMENT);
        testAssignmentOperatorExpression("foo +>> -5", TokenType.RIGHT_ADD_ASSIGNMENT);
        testAssignmentOperatorExpression("foo ->> -5", TokenType.RIGHT_SUBTRACT_ASSIGNMENT);
        testAssignmentOperatorExpression("foo \\>> -5", TokenType.RIGHT_DIVIDE_ASSIGNMENT);
        testAssignmentOperatorExpression("foo *>> -5", TokenType.RIGHT_MULTIPLY_ASSIGNMENT);
    }

    @Test
    @Order(14)
    public void testLexer_Crystals_NegationOperator_AfterLogicalAssignmentOperators() {
        testLogicalOperatorExpression("foo ^<< -5 = bar", TokenType.LEFT_LOGICAL_AND_ASSIGNMENT);
        testLogicalOperatorExpression("foo \"<< -5 = bar", TokenType.LEFT_LOGICAL_OR_ASSIGNMENT);
        testLogicalOperatorExpression("foo ^>> -5 = bar", TokenType.RIGHT_LOGICAL_AND_ASSIGNMENT);
        testLogicalOperatorExpression("foo \">> -5 = bar", TokenType.RIGHT_LOGICAL_OR_ASSIGNMENT);
    }

    @Test
    @Order(15)
    public void testLexer_Crystals_NegationOperator_AfterBinaryLogicalOperators() {
        testLogicalOperatorExpression("foo ^ -5 = bar", TokenType.LOGICAL_AND);
        testLogicalOperatorExpression("foo \" -5 = bar", TokenType.LOGICAL_OR);
    }

    @Test
    @Order(16)
    public void testLexer_Crystals_NegationOperator_AfterLogicalNotOperator() {
        List<AtonementCrystal> statement = lexSingleStatement("'-5 = bar", 4);

        testCrystal(statement.get(0), LogicalNotOperatorCrystal.class, "'", location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "-5", location(0, 2));
        testCrystal(statement.get(2), EqualsOperatorCrystal.class, "=", location(0, 4));
        testCrystal(statement.get(3), ReferenceCrystal.class, "bar", location(0, 6));

        testNegationOperatorLocation(statement.get(1), location(0, 1));
    }

    @Test
    @Order(17)
    public void testLexer_Crystals_NegationOperator_AfterComparisonOperators() {
        testBinaryOperatorExpression("foo = -5", TokenType.EQUALS);
        testBinaryOperatorExpression("foo <=> -5", TokenType.REFERENCE_EQUALS);
        testBinaryOperatorExpression("foo < -5", TokenType.LESS_THAN);
        testBinaryOperatorExpression("foo > -5", TokenType.GREATER_THAN);
        testBinaryOperatorExpression("foo >= -5", TokenType.GREATER_THAN_OR_EQUALS);
        testBinaryOperatorExpression("foo <= -5", TokenType.LESS_THAN_OR_EQUALS);
    }

    @Test
    @Order(18)
    public void testLexer_Crystals_NegationOperator_AfterLeftFeatherFallOperator() {
        List<AtonementCrystal> statement = lexSingleStatement("\\\\ -5 / 0 //", 5);

        testCrystal(statement.get(0), LeftFeatherFallCrystal.class, "\\\\", location(0, 0));
        testCrystal(statement.get(1), IntegerCrystal.class, "-5", location(0, 4));
        testCrystal(statement.get(2), LeftDivideOperatorCrystal.class, "/", location(0, 6));
        testCrystal(statement.get(3), IntegerCrystal.class, "0", location(0, 8));
        testCrystal(statement.get(4), RightFeatherFallCrystal.class, "//", location(0, 10));

        testNegationOperatorLocation(statement.get(1), location(0, 3));
    }

    @Test
    @Order(19)
    public void testLexer_Crystals_NegationOperator_AfterRightFeatherFallOperator() {
        List<AtonementCrystal> statement = lexSingleStatement("\\\\ dangerous!() // - 5", 8);

        testCrystal(statement.get(0), LeftFeatherFallCrystal.class, "\\\\", location(0, 0));
        testCrystal(statement.get(1), ReferenceCrystal.class, "dangerous", location(0, 3));
        testCrystal(statement.get(2), FunctionCallOperatorCrystal.class, "!", location(0, 12));
        testCrystal(statement.get(3), LeftParenthesisCrystal.class, "(", location(0, 13));
        testCrystal(statement.get(4), RightParenthesisCrystal.class, ")", location(0, 14));
        testCrystal(statement.get(5), RightFeatherFallCrystal.class, "//", location(0, 16));
        testCrystal(statement.get(6), SubtractOperatorCrystal.class, "-", location(0, 19));
        testCrystal(statement.get(7), IntegerCrystal.class, "5", location(0, 21));
    }

    @Test
    @Order(20)
    public void testLexer_Crystals_NegationOperator_WithSingleLineComment_BeforeNegation() {
        List<AtonementCrystal> statement = lexSingleStatement("2 + ~:Comment.:~ -5", 3);

        testCrystal(statement.get(0), IntegerCrystal.class, "2", location(0, 0));
        testCrystal(statement.get(1), AddOperatorCrystal.class, "+", location(0, 2));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(0, 18));

        testNegationOperatorLocation(statement.get(2), location(0, 17));
    }

    @Test
    @Order(21)
    public void testLexer_Crystals_NegationOperator_WithSingleLineComment_AfterNegation() {
        List<AtonementCrystal> statement = lexSingleStatement("2 + -~:Comment.:~5", 3);

        testCrystal(statement.get(0), IntegerCrystal.class, "2", location(0, 0));
        testCrystal(statement.get(1), AddOperatorCrystal.class, "+", location(0, 2));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(0, 17));

        testNegationOperatorLocation(statement.get(2), location(0, 4));
    }

    @Test
    @Order(22)
    public void testLexer_Crystals_NegationOperator_WithMultiLineComment_BeforeNegation() {
        List<AtonementCrystal> statement = lexSingleStatement("2 + ~:Multi-line\ncomment.:~ -5", 3);

        testCrystal(statement.get(0), IntegerCrystal.class, "2", location(0, 0));
        testCrystal(statement.get(1), AddOperatorCrystal.class, "+", location(0, 2));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(1, 12));

        testNegationOperatorLocation(statement.get(2), location(1, 11));
    }

    @Test
    @Order(23)
    public void testLexer_Crystals_NegationOperator_WithMultiLineComment_AfterNegation() {
        List<AtonementCrystal> statement = lexSingleStatement("2 + -~:Multi-line\ncomment.:~5", 3);

        testCrystal(statement.get(0), IntegerCrystal.class, "2", location(0, 0));
        testCrystal(statement.get(1), AddOperatorCrystal.class, "+", location(0, 2));
        testCrystal(statement.get(2), IntegerCrystal.class, "-5", location(1, 10));

        testNegationOperatorLocation(statement.get(2), location(0, 4));
    }

    @Test
    @Order(24)
    public void testLexer_Crystals_NegationOperator_AfterFunctionCallOperator() {
        List<AtonementCrystal> statement = lexSingleStatement("crystal.castToInteger! - 5", 6);

        testCrystal(statement.get(0), ReferenceCrystal.class, "crystal", location(0, 0));
        testCrystal(statement.get(1), DotOperatorCrystal.class, ".", location(0, 7));
        testCrystal(statement.get(2), ReferenceCrystal.class, "castToInteger", location(0, 8));
        testCrystal(statement.get(3), FunctionCallOperatorCrystal.class, "!", location(0, 21));
        testCrystal(statement.get(4), SubtractOperatorCrystal.class, "-", location(0, 23));
        testCrystal(statement.get(5), IntegerCrystal.class, "5", location(0, 25));
    }
}
