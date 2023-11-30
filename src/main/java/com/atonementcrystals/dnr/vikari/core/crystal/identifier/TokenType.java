package com.atonementcrystals.dnr.vikari.core.crystal.identifier;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.CollectionLiteralOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.ExistsOperatorCrystal;
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
import com.atonementcrystals.dnr.vikari.core.crystal.operator.DotOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.FunctionCallOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.KeyValuePairOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.MinimizedLineContinuationCrystal;
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
import com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison.NotEqualsOperatorCrystal;
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
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.NegateOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.RightDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.AnnotationOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.CastOperatorCrystal;
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
import com.atonementcrystals.dnr.vikari.core.crystal.separator.LeftSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RightSquareBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.LeftParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.ListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.RightParenthesisCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.annotation.AnnotationClosingBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.annotation.AnnotationElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.annotation.AnnotationOpeningBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal.ArrayLiteralClosingBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal.ArrayLiteralOpeningBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal.CollectionLiteralElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal.ListLiteralClosingBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal.ListLiteralOpeningBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal.SetLiteralClosingBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal.SetLiteralOpeningBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.function.FunctionArgumentListClosingBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.function.FunctionArgumentListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.function.FunctionArgumentListOpeningBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.function.FunctionParameterListClosingBracketCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.function.FunctionParameterListElementSeparatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.function.FunctionParameterListOpeningBracketCrystal;
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

    // Comments
    COMMENT_PREFIX_CRYSTAL("~:", null),
    COMMENT_SUFFIX_CRYSTAL(":~", null),

    // Control flow
    CONDITIONAL_BRANCH("??", ConditionalBranchCrystal.class),
    LOOP("<>", LoopCrystal.class),
    THROW("--", ThrowCrystal.class),
    CATCH("++", CatchCrystal.class),

    RETURN("^^", ReturnOperatorCrystal.class),
    CONTINUE(">>", ContinueOperatorCrystal.class),
    BREAK("vv", BreakOperatorCrystal.class),

    // Separators
    BACKTICK("`", BacktickQuotationCrystal.class),
    CAPTURE_QUOTATION("``", CaptureQuotationCrystal.class),
    LEFT_PARENTHESIS("(",LeftParenthesisCrystal.class),
    RIGHT_PARENTHESIS(")", RightParenthesisCrystal.class),
    LEFT_CURLY_BRACKET("{", LeftCurlyBracketCrystal.class),
    RIGHT_CURLY_BRACKET("}", RightCurlyBracketCrystal.class),
    LEFT_SQUARE_BRACKET("[", LeftSquareBracketCrystal.class),
    RIGHT_SQUARE_BRACKET("]", RightSquareBracketCrystal.class),
    LIST_ELEMENT_SEPARATOR("|", ListElementSeparatorCrystal.class),

    REGION_OPERATOR("::", RegionOperatorCrystal.class),
    STATEMENT_SEPARATOR(",", StatementSeparatorCrystal.class),
    REGION_SEPARATOR(";", RegionSeparatorCrystal.class),

    // Function parameter and argument lists
    FUNCTION_PARAMETER_LIST_OPENING_BRACKET("(", FunctionParameterListOpeningBracketCrystal.class),
    FUNCTION_PARAMETER_LIST_CLOSING_BRACKET(")", FunctionParameterListClosingBracketCrystal.class),
    FUNCTION_PARAMETER_LIST_ELEMENT_SEPARATOR("|", FunctionParameterListElementSeparatorCrystal.class),

    FUNCTION_ARGUMENT_LIST_OPENING_BRACKET("(", FunctionArgumentListOpeningBracketCrystal.class),
    FUNCTION_ARGUMENT_LIST_CLOSING_BRACKET(")", FunctionArgumentListClosingBracketCrystal.class),
    FUNCTION_ARGUMENT_LIST_ELEMENT_SEPARATOR("|", FunctionArgumentListElementSeparatorCrystal.class),

    // List constructor literals
    COLLECTION_LITERAL("$:", CollectionLiteralOperatorCrystal.class),
    LIST_LITERAL_OPENING_BRACKET("(", ListLiteralOpeningBracketCrystal.class),
    LIST_LITERAL_CLOSING_BRACKET(")", ListLiteralClosingBracketCrystal.class),
    SET_LITERAL_OPENING_BRACKET("{", SetLiteralOpeningBracketCrystal.class),
    SET_LITERAL_CLOSING_BRACKET("}", SetLiteralClosingBracketCrystal.class),
    ARRAY_LITERAL_OPENING_BRACKET("[", ArrayLiteralOpeningBracketCrystal.class),
    ARRAY_LITERAL_CLOSING_BRACKET("]",ArrayLiteralClosingBracketCrystal.class),
    COLLECTION_LITERAL_ELEMENT_SEPARATOR("|", CollectionLiteralElementSeparatorCrystal.class),

    // Annotations
    ANNOTATION("$:", AnnotationOperatorCrystal.class),
    ANNOTATION_OPENING_BRACKET("{", AnnotationOpeningBracketCrystal.class),
    ANNOTATION_CLOSING_BRACKET("}", AnnotationClosingBracketCrystal.class),
    ANNOTATION_ELEMENT_SEPARATOR("|", AnnotationElementSeparatorCrystal.class),

    // Operators
    DOT(".", DotOperatorCrystal.class),
    RANGE("..", RangeOperatorCrystal.class),
    VARIABLE_ARGUMENTS_LIST("...", VariableArgumentsListOperatorCrystal.class),
    TYPE_LABEL(":", TypeLabelOperatorCrystal.class),
    PRINT_STATEMENT(":", PrintStatementOperatorCrystal.class),
    FUNCTION_CALL("!", FunctionCallOperatorCrystal.class),
    CAST("!", CastOperatorCrystal.class),

    INSTANCE_FIELD_ACCESS("@", InstanceFieldAccessOperatorCrystal.class),
    INSTANCE_SUPER("@", InstanceSuperOperatorCrystal.class),
    STATIC_FIELD_ACCESS("#", StaticFieldAccessOperatorCrystal.class),
    STATIC_SUPER("#", StaticSuperOperatorCrystal.class),
    INDEX_OPERATOR("$", IndexOperatorCrystal.class),

    COPY_CONSTRUCTOR("&", CopyConstructorCrystal.class),
    MODULUS("%", ModulusOperatorCrystal.class),
    MULTIPLY("*", MultiplyOperatorCrystal.class),
    SUBTRACT("-", SubtractOperatorCrystal.class),
    NEGATE("-", NegateOperatorCrystal.class),
    EXISTS("?", ExistsOperatorCrystal.class),
    INSTANCE_OF("?", InstanceOfOperatorCrystal.class),

    ADD("+", AddOperatorCrystal.class),
    CONCATENATE("+", ConcatenateOperatorCrystal.class),
    LEFT_DIVIDE("/", LeftDivideOperatorCrystal.class),
    RIGHT_DIVIDE("\\", RightDivideOperatorCrystal.class),
    DELETE("~", DeleteOperatorCrystal.class),
    LINE_CONTINUATION("~", LineContinuationCrystal.class),
    MINIMIZED_LINE_CONTINUATION("/~/", MinimizedLineContinuationCrystal.class),

    // Assignment operators
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
    LEFT_LOGICAL_OR_ASSIGNMENT("\"<<", LeftLogicalOrAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_AND_ASSIGNMENT("^>>", RightLogicalAndAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_OR_ASSIGNMENT("\">>", RightLogicalOrAssignmentOperatorCrystal.class),

    // Logical and comparison operators
    LOGICAL_AND("^", LogicalAndOperatorCrystal.class),
    LOGICAL_OR("\"", LogicalOrOperatorCrystal.class),
    LOGICAL_NOT("'", LogicalNotOperatorCrystal.class),
    EQUALS("=", EqualsOperatorCrystal.class),
    NOT_EQUALS("'=", NotEqualsOperatorCrystal.class),

    // TODO: Determine default behavior of reference equality for literal values.
    REFERENCE_EQUALS("<=>", ReferenceEqualsOperatorCrystal.class),

    // Numeric comparisons
    GREATER_THAN(">", GreaterThanOperatorCrystal.class),
    LESS_THAN("<", LessThanOperatorCrystal.class),
    GREATER_THAN_OR_EQUALS(">=", GreaterThanOrEqualsOperatorCrystal.class),
    LESS_THAN_OR_EQUALS("<=", LessThanOrEqualsOperatorCrystal.class),

    // Miscellaneous
    KEY_VALUE_PAIR("=>", KeyValuePairOperatorCrystal.class),
    ITERATION_ELEMENT("<-", IterationElementOperatorCrystal.class),
    SWORD("_", SwordCrystal.class),

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
            TokenType.ANNOTATION_ELEMENT_SEPARATOR,
            // special cases
            TokenType.SWORD,
            TokenType.THROW,
            TokenType.LINE_CONTINUATION,
            TokenType.STATEMENT_SEPARATOR);

    /**
     * This is the set of all TokenTypes to be handled by the Lexer.
     */
    public static final Set<TokenType> LEXER_TOKENS = Arrays.stream(TokenType.values())
            .filter(Predicate.not(DUPLICATES_AND_SPECIAL_CASES::contains))
            .collect(Collectors.toCollection(() -> EnumSet.noneOf(TokenType.class)));

    /**
     * Create a new TokenType with a mapping between its string identifier and its
     * concrete Crystal class type.
     *
     * @param identifier The default string identifier.
     * @param javaType The concrete crystal class type.
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
     * @return The concrete crystal class type for the token.
     *         If null, this TokenType is not used directly by the parser,
     *         but rather is used to combine into a larger crystal.
     */
    public Class<? extends AtonementCrystal> getJavaType() {
        return javaType;
    }
}
