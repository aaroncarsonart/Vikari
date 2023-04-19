package com.atonement.crystals.dnr.vikari.core.identifier;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.control.flow.ConditionalBranchCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.control.flow.LoopCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.error.CatchCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.error.ThrowCrystal;
import com.atonement.crystals.dnr.vikari.core.literal.SwordCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.ConcatenateOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.ConstructorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.DotOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.FunctionCallOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.KeyValuePairOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.LineContinuationMinimizedOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.LineContinuationOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.PrintStatementOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.TypeLabelOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.VariableArgumentsListOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.angelguard.CatchAllCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.angelguard.LeftFeatherFallCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.angelguard.RightFeatherFallCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.RightAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.LeftLogicalAndAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.LeftOrAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.RightAndAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.RightOrAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftAddAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftDivideAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftMultiplyAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftSubtractAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.RightAddAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.RightDivideAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.RightMultiplyAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.RightSubtractAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.EqualsOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.GreaterThanOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.GreaterThanOrEqualsOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.InstanceOfOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.LessThanOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.LessThanOrEqualsOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.ReferenceEqualsOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.control.flow.BreakOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.control.flow.ContinueOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.control.flow.IterationElementOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.control.flow.ReturnOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.logical.LogicalAndOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.logical.LogicalNotOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.logical.LogicalOrOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.AddOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.LeftDivideOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.ModulusOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.MultiplyOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.NegateCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.PercentCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.RightDivideOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.SubtractCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.AnnotationOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.CopyConstructorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.DeleteOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.IndexOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.InstanceFieldAccessOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.InstanceSuperOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.RangeOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.StaticFieldAccessOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.StaticSuperOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.LeftCurlyBracketCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.RegionOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.RegionSeparatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.RightCurlyBracketCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.StatementSeparatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.grouping.LeftSquareBracketCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.grouping.RightSquareBracketCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.list.LeftParenthesisCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.list.ListElementSeparatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.list.RightParenthesisCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.quotation.CaptureQuotationCrystal;

import java.util.Arrays;
import java.util.EnumSet;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Defines a mapping for all Vikari language symbols to their concrete crystal class types.
 * Tokens with a null type are never instantiated directly as crystals, but rather used
 * instead as a part of larger crystal token.
 */
public enum TokenType {

    // comments
    COMMENT_PREFIX_CRYSTAL("~:", null),
    COMMENT_SUFFIX_CRYSTAL(":~", null),

    // control flow
    CONDITIONAL_BRANCH("??", ConditionalBranchCrystal.class),
    LOOP("<>", LoopCrystal.class),
    THROW("--", ThrowCrystal.class),
    CATCH("++", CatchCrystal.class),

    RETURN("^^", ReturnOperatorCrystal.class),
    CONTINUE(">>", ContinueOperatorCrystal.class),
    BREAK("vv", BreakOperatorCrystal.class),

    // literals
    SWORD("_", SwordCrystal.class),

    // Separators
    BACKTICK("`", null),
    CAPTURE_QUOTATION("``", CaptureQuotationCrystal.class),
    LEFT_SQUARE_BRACKET("[", LeftSquareBracketCrystal.class),
    RIGHT_SQUARE_BRACKET("]", RightSquareBracketCrystal.class),

    REGION_OPERATOR("::", RegionOperatorCrystal.class),
    STATEMENT_SEPARATOR(",", StatementSeparatorCrystal.class),
    REGION_SEPARATOR(";", RegionSeparatorCrystal.class),

    // List constructor literals
    LEFT_PARENTHESIS("(",LeftParenthesisCrystal.class),
    RIGHT_PARENTHESIS(")", RightParenthesisCrystal.class),
    LIST_ELEMENT_SEPARATOR("|", ListElementSeparatorCrystal.class),

    // Atonement Field projection enclosures
    LEFT_CURLY_BRACKET("{", LeftCurlyBracketCrystal.class),
    RIGHT_CURLY_BRACKET("}", RightCurlyBracketCrystal.class),

    // Operators
    DOT(".", DotOperatorCrystal.class),
    RANGE("..", RangeOperatorCrystal.class),
    VARIABLE_ARGUMENTS_LIST("...", VariableArgumentsListOperatorCrystal.class),
    TYPE_LABEL(":", TypeLabelOperatorCrystal.class),
    PRINT_STATEMENT(":", PrintStatementOperatorCrystal.class),
    FUNCTION_CALL("!", FunctionCallOperatorCrystal.class),

    INSTANCE_FIELD_ACCESS("@", InstanceFieldAccessOperatorCrystal.class),
    INSTANCE_SUPER("@", InstanceSuperOperatorCrystal.class),
    STATIC_FIELD_ACCESS("#", StaticFieldAccessOperatorCrystal.class),
    STATIC_SUPER("#", StaticSuperOperatorCrystal.class),
    INDEX_OPERATOR("$", IndexOperatorCrystal.class),
    ANNOTATION("$", AnnotationOperatorCrystal.class),
    COPY_CONSTRUCTOR("&", CopyConstructorCrystal.class),
    MODULUS("%", ModulusOperatorCrystal.class),
    PERCENT("%", PercentCrystal.class),
    MULTIPLY("*", MultiplyOperatorCrystal.class),
    CONSTRUCTOR("*", ConstructorCrystal.class),
    SUBTRACT("-", SubtractCrystal.class),
    NEGATE("-", NegateCrystal.class),

    // assignment operators
    LEFT_ASSIGNMENT("<<", LeftAssignmentOperatorCrystal.class),
    LEFT_ADD_ASSIGNMENT("+<<", LeftAddAssignmentOperatorCrystal.class),
    LEFT_SUBTRACT_ASSIGNMENT("-<<", LeftSubtractAssignmentOperatorCrystal.class),
    LEFT_DIVIDE_ASSIGNMENT("/<<", LeftDivideAssignmentOperatorCrystal.class),
    LEFT_MULTIPLY_ASSIGNMENT("*<<", LeftMultiplyAssignmentOperatorCrystal.class),

    RIGHT_ASSIGNMENT(">>", RightAssignmentOperatorCrystal.class),
    RIGHT_ADD_ASSIGNMENT("+>>", RightAddAssignmentOperatorCrystal.class),
    RIGHT_SUBTRACT_ASSIGNMENT("->>", RightSubtractAssignmentOperatorCrystal.class),
    RIGHT_DIVIDE_ASSIGNMENT("\\>>", RightDivideAssignmentOperatorCrystal.class),
    RIGHT_MULTIPLY_ASSIGNMENT("*>>", RightMultiplyAssignmentOperatorCrystal.class),

    LEFT_LOGICAL_AND_ASSIGNMENT("^<<", LeftLogicalAndAssignmentOperatorCrystal.class),
    LEFT_LOGICAL_OR_ASSIGNMENT("\"<<", LeftOrAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_AND_ASSIGNMENT("^>>", RightAndAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_OR_ASSIGNMENT("\">>", RightOrAssignmentOperatorCrystal.class),

    ADD("+", AddOperatorCrystal.class),
    CONCATENATE("+", ConcatenateOperatorCrystal.class),
    LEFT_DIVIDE("/", LeftDivideOperatorCrystal.class),
    RIGHT_DIVIDE("\\", RightDivideOperatorCrystal.class),
    DELETE("~", DeleteOperatorCrystal.class),
    LINE_CONTINUATION("~", LineContinuationOperatorCrystal.class),
    LINE_CONTINUATION_MINIMIZED("/~/", LineContinuationMinimizedOperatorCrystal.class),

    // logical and comparison operators
    LOGICAL_AND("^", LogicalAndOperatorCrystal.class),
    LOGICAL_OR("\"", LogicalOrOperatorCrystal.class),
    LOGICAL_NOT("'", LogicalNotOperatorCrystal.class),
    EQUALS("=", EqualsOperatorCrystal.class),

    // TODO: Determine default behavior of reference equality for literal values.
    REFERENCE_EQUALS("<=>", ReferenceEqualsOperatorCrystal.class),

    // numeric comparisons
    GREATER_THAN("<", GreaterThanOperatorCrystal.class),
    LESS_THAN(">", LessThanOperatorCrystal.class),
    GREATER_THAN_OR_EQUALS(">=", GreaterThanOrEqualsOperatorCrystal.class),
    LESS_THAN_OR_EQUALS("<=", LessThanOrEqualsOperatorCrystal.class),

    // miscellaneous
    KEY_VALUE_PAIR("=>", KeyValuePairOperatorCrystal.class),
    ITERATION_ELEMENT("<-", IterationElementOperatorCrystal.class),
    INSTANCE_OF("->", InstanceOfOperatorCrystal.class),

    // Angel guards
    CATCH_ALL("||", CatchAllCrystal.class),
    LEFT_FEATHER_FALL("\\\\", LeftFeatherFallCrystal.class),
    RIGHT_FEATHER_FALL("//", RightFeatherFallCrystal.class);

    /**
     * This is a set of all token types which are overloads to be ignored by the
     * Lexer. All tokens will be resolved by their counterparts instead, as listed
     * before the removed TokenType in the enum values list declaration of this
     * class.
     */
    public static final Set<TokenType> DUPLICATES_AND_SPECIAL_CASES = EnumSet.of(
            // duplicates
            TokenType.INSTANCE_SUPER,
            TokenType.STATIC_SUPER,
            TokenType.ANNOTATION,
            TokenType.PERCENT,
            TokenType.CONSTRUCTOR,
            TokenType.NEGATE,
            TokenType.RIGHT_ASSIGNMENT,
            TokenType.CONCATENATE,
            TokenType.LINE_CONTINUATION,
            TokenType.PRINT_STATEMENT,
            // special cases
            TokenType.BREAK,
            TokenType.SWORD);

    /**
     * This is the set of all TokenTypes to be handled by the Lexer.
     */
    public static final Set<TokenType> LEXER_TOKENS = Arrays.stream(TokenType.values())
            .filter(Predicate.not(DUPLICATES_AND_SPECIAL_CASES::contains))
            .collect(Collectors.toCollection(() -> EnumSet.noneOf(TokenType.class)));

    /**
     * Create A new TokenType with a mapping between its string identifier and its
     * concrete Crystal class type.
     *
     * @param identifier The default string identifier.
     * @param type The concrete Crystal class type.
     */
    TokenType(String identifier, Class<? extends AtonementCrystal> type) {
        this.identifier = identifier;
        this.type = type;
    }

    private final String identifier;
    private final Class<? extends AtonementCrystal> type;

    /**
     * @return The token's default string representation in Vikari.
     */
    public String getIdentifier() {
        return identifier;
    }

    /**
     * @return The concrete Crystal class type for the token.
     *         If null, this TokenType is not used directly by the parser,
     *         but rather is used to combine into a larger crystal.
     */
    public Class<? extends AtonementCrystal> getType() {
        return type;
    }
}
