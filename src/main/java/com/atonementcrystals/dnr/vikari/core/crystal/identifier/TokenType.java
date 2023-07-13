package com.atonementcrystals.dnr.vikari.core.crystal.identifier;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.NullCoalesceOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftNullCoalesceAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.RightNullCoalesceAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.bool.LeftLogicalAndAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.LeftMultiplyAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.RightAddAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.RightDivideAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.RightMultiplyAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.control.flow.ConditionalBranchCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.control.flow.LoopCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.error.CatchCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.error.ThrowCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.SwordCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.ConcatenateOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.ConstructorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.DotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.KeyValuePairOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.LineContinuationMinimizedOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.LineContinuationCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.PrintStatementOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.VariableArgumentsListOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.angelguard.CatchAllCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.angelguard.LeftFeatherFallCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.angelguard.RightFeatherFallCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.RightAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.bool.LeftLogicalOrAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.bool.RightLogicalAndAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.bool.RightLogicalOrAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.LeftAddAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.LeftDivideAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.LeftSubtractAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math.RightSubtractAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.EqualsOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.GreaterThanOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.GreaterThanOrEqualsOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.InstanceOfOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.LessThanOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.LessThanOrEqualsOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.ReferenceEqualsOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow.BreakOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow.ContinueOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow.IterationElementOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow.ReturnOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalAndOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalNotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalOrOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.ModulusOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.NegateCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.PercentCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.RightDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.AnnotationOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.CopyConstructorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.DeleteOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.IndexOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.InstanceFieldAccessOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.InstanceSuperOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.RangeOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.StaticFieldAccessOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.StaticSuperOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.LeftCurlyBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RegionOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RegionSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RightCurlyBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.StatementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.LeftSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping.RightSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.ListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.list.RightParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.quotation.BacktickQuotationCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.quotation.CaptureQuotationCrystal;

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
    BACKTICK("`", BacktickQuotationCrystal.class),
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
    SUBTRACT("-", SubtractOperatorCrystal.class),
    NEGATE("-", NegateCrystal.class),
    NULL_COALESCE("?", NullCoalesceOperatorCrystal.class),

    // assignment operators
    LEFT_ASSIGNMENT("<<", LeftAssignmentOperatorCrystal.class),
    LEFT_ADD_ASSIGNMENT("+<<", LeftAddAssignmentOperatorCrystal.class),
    LEFT_SUBTRACT_ASSIGNMENT("-<<", LeftSubtractAssignmentOperatorCrystal.class),
    LEFT_DIVIDE_ASSIGNMENT("/<<", LeftDivideAssignmentOperatorCrystal.class),
    LEFT_MULTIPLY_ASSIGNMENT("*<<", LeftMultiplyAssignmentOperatorCrystal.class),
    LEFT_NULL_COALESCE_ASSIGNMENT("?<<", LeftNullCoalesceAssignmentOperatorCrystal.class),

    RIGHT_ASSIGNMENT(">>", RightAssignmentOperatorCrystal.class),
    RIGHT_ADD_ASSIGNMENT("+>>", RightAddAssignmentOperatorCrystal.class),
    RIGHT_SUBTRACT_ASSIGNMENT("->>", RightSubtractAssignmentOperatorCrystal.class),
    RIGHT_DIVIDE_ASSIGNMENT("\\>>", RightDivideAssignmentOperatorCrystal.class),
    RIGHT_MULTIPLY_ASSIGNMENT("*>>", RightMultiplyAssignmentOperatorCrystal.class),
    RIGHT_NULL_COALESCE_ASSIGNMENT("?>>", RightNullCoalesceAssignmentOperatorCrystal.class),

    LEFT_LOGICAL_AND_ASSIGNMENT("^<<", LeftLogicalAndAssignmentOperatorCrystal.class),
    LEFT_LOGICAL_OR_ASSIGNMENT("\"<<", LeftLogicalOrAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_AND_ASSIGNMENT("^>>", RightLogicalAndAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_OR_ASSIGNMENT("\">>", RightLogicalOrAssignmentOperatorCrystal.class),

    ADD("+", AddOperatorCrystal.class),
    CONCATENATE("+", ConcatenateOperatorCrystal.class),
    LEFT_DIVIDE("/", LeftDivideOperatorCrystal.class),
    RIGHT_DIVIDE("\\", RightDivideOperatorCrystal.class),
    DELETE("~", DeleteOperatorCrystal.class),
    LINE_CONTINUATION("~", LineContinuationCrystal.class),
    LINE_CONTINUATION_MINIMIZED("/~/", LineContinuationMinimizedOperatorCrystal.class),

    // logical and comparison operators
    LOGICAL_AND("^", LogicalAndOperatorCrystal.class),
    LOGICAL_OR("\"", LogicalOrOperatorCrystal.class),
    LOGICAL_NOT("'", LogicalNotOperatorCrystal.class),
    EQUALS("=", EqualsOperatorCrystal.class),

    // TODO: Determine default behavior of reference equality for literal values.
    REFERENCE_EQUALS("<=>", ReferenceEqualsOperatorCrystal.class),

    // numeric comparisons
    GREATER_THAN(">", GreaterThanOperatorCrystal.class),
    LESS_THAN("<", LessThanOperatorCrystal.class),
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
            TokenType.DELETE,
            TokenType.PRINT_STATEMENT,
            // special cases
            TokenType.SWORD,
            TokenType.THROW,
            TokenType.LINE_CONTINUATION);

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
     * @param javaType The concrete Crystal class type.
     */
    TokenType(String identifier, Class<? extends AtonementCrystal> javaType) {
        this.identifier = identifier;
        this.javaType = javaType;
    }

    private final String identifier;
    private final Class<? extends AtonementCrystal> javaType;

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
    public Class<? extends AtonementCrystal> getJavaType() {
        return javaType;
    }
}
