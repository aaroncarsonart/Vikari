package com.atonement.crystals.dnr.vikari.core.identifier;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.FunctionOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.comment.CommentPrefixCrystal;
import com.atonement.crystals.dnr.vikari.core.comment.CommentSuffixCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.ReturnOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.error.CatchCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.ConditionalBranchCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.label.ImportCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.LoopCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.label.PrivateAccessModifierCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.label.PublicAccessModifierCrystal;
import com.atonement.crystals.dnr.vikari.core.keyword.error.ThrowCrystal;
import com.atonement.crystals.dnr.vikari.core.literal.BooleanLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.literal.SwordCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.angelguard.CatchAllCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.ConstructorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.KeyValuePairOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.angelguard.LeftFeatherFallCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.LineContinuationOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.angelguard.RightFeatherFallCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.EqualsComparisonOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.GreaterThanComparisonOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.GreaterThanOrEqualsComparisonOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.LessThanComparisonOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.comparison.LessThanOrEqualsComparisonOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.curriculum.StudentOfStudentsCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.curriculum.TeacherOfTeachersCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.logical.LogicalAndOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.logical.LogicalOrOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.CopyConstructorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.DeleteOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.DotOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.Ecalyippe_Nipyonne_Dyumdyennai_Crystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.FieldMemberAccessPrefixCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.HarmonizedFieldMemberAccessPrefixCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.IndexOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.KnowledgeOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.LeftDivideOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.ModulusOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.MultiplyOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.NegateCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.logical.LogicalNotOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.PercentCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.RightDivideOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.SubtractCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.AddOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.ConcatenateOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.FeatherCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftAddAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.LeftLogicalAndAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftDivideAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftMultiplyAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.LeftOrAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.LeftSubtractAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.RightAddAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.RightAndAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.RightAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.RightMultiplyAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.bool.RightOrAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.math.RightSubtractAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.EscapedLeftCaptureQuotationCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.emotion.FearCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.emotion.LeftFearCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.emotion.LoveCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.emotion.RightFearCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.quotation.BacktickCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.quotation.CaptureQuotationCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.LeftCurlyBracketCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.list.LeftParenthesisCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.list.ListElementSeparatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.MultipleStatementSeparatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.QuaternityOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.RightCurlyBracketCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.list.RightParenthesisCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.LeftSquareBracketCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.quotation.EscapedRightCaptureQuotationCrystal;

public enum DefaultIdentifierMapping {

    // comments
    COMMENT_PREFIX_CRYSTAL("~:", CommentPrefixCrystal.class),
    COMMENT_SUFFIX_CRYSTAL(":~", CommentSuffixCrystal.class),

    // Keywords
    CONDITIONAL_BRANCH("??", ConditionalBranchCrystal.class),
    LOOP("<>", LoopCrystal.class),
    THROW("--", ThrowCrystal.class),
    CATCH("++", CatchCrystal.class),
    PUBLIC_ACCESS_MODIFIER("public", PublicAccessModifierCrystal.class),
    PRIVATE_ACCESS_MODIFIER("private", PrivateAccessModifierCrystal.class),
    IMPORT("import", ImportCrystal.class),

    // Literals
    TRUE("true", BooleanLiteralCrystal.class),
    FALSE("false", BooleanLiteralCrystal.class),
    SWORD("_", SwordCrystal.class),

    // Separators
    BACKTICK("`", BacktickCrystal.class),
    CAPTURE_QUOTATION("``", CaptureQuotationCrystal.class),
    LEFT_SQUARE_BRACKET("[", LeftSquareBracketCrystal.class),
    RIGHT_SQUARE_BRACKET("]", LeftSquareBracketCrystal.class),
    ESCAPED_LEFT_CAPTURE_QUOTATION("[``", EscapedLeftCaptureQuotationCrystal.class),
    ESCAPED_RIGHT_CAPTURE_QUOTATION("``]", EscapedRightCaptureQuotationCrystal.class),

    MULTIPLE_STATEMENT_SEPARATOR(",", MultipleStatementSeparatorCrystal.class),
    QUATERNITY_OPERATOR("::", QuaternityOperatorCrystal.class),

    // List constructor literals
    LEFT_PARENTHESIS("(",LeftParenthesisCrystal .class),
    RIGHT_PARENTHESIS(")", RightParenthesisCrystal.class),
    LIST_ELEMENT_SEPARATOR("|", ListElementSeparatorCrystal.class),

    // Atonement Field projection enclosures
    LEFT_CURLY_BRACKET("{", LeftCurlyBracketCrystal.class),
    RIGHT_CURLY_BRACKET("}", RightCurlyBracketCrystal.class),

    // Operators
    DOT_OPERATOR(".", DotOperatorCrystal.class),
    ECALYIPPE_NIPYONNE_DYUMDYENNAII("...", Ecalyippe_Nipyonne_Dyumdyennai_Crystal.class),
    KNOWLEDGE_OPERATOR(":", KnowledgeOperatorCrystal.class),
    FUNCTION_OPERATOR("!", FunctionOperatorCrystal.class),

    FIELD_MEMBER_ACCESS("@", FieldMemberAccessPrefixCrystal.class),
    HARMONIZED_FIELD_MEMBER_ACCESS("#", HarmonizedFieldMemberAccessPrefixCrystal.class),
    INDEX_OPERATOR("$", IndexOperatorCrystal.class),
    COPY_CONSTRUCTOR("&", CopyConstructorCrystal.class),
    PERCENT("%", PercentCrystal.class),
    MODULUS("%", ModulusOperatorCrystal.class),
    MULTIPLY("*", MultiplyOperatorCrystal.class),
    CONSTRUCTOR("*", ConstructorCrystal.class),
    NEGATE("-", NegateCrystal.class),
    SUBTRACT("-", SubtractCrystal.class),

    // assignment operators
    LEFT_ASSIGNMENT("<<", LeftAssignmentOperatorCrystal.class),
    LEFT_ADD_ASSIGNMENT("+<<", LeftAddAssignmentOperatorCrystal.class),
    LEFT_SUBTRACT_ASSIGNMENT("-<<", LeftSubtractAssignmentOperatorCrystal.class),
    LEFT_DIVIDE_ASSIGNMENT("/<<", LeftDivideAssignmentOperatorCrystal.class),
    LEFT_MULTIPLY_ASSIGNMENT("*<<", LeftMultiplyAssignmentOperatorCrystal.class),

    RIGHT_ASSIGNMENT(">>", RightAssignmentOperatorCrystal.class),
    RIGHT_ADD_ASSIGNMENT("+>>", RightAddAssignmentOperatorCrystal.class),
    RIGHT_SUBTRACT_ASSIGNMENT("->>", RightSubtractAssignmentOperatorCrystal.class),
    RIGHT_DIVIDE_ASSIGNMENT("\\>>", RightDivideOperatorCrystal.class),
    RIGHT_MULTIPLY_ASSIGNMENT("*>>", RightMultiplyAssignmentOperatorCrystal.class),

    LEFT_LOGICAL_AND_ASSIGNMENT("^<<", LeftLogicalAndAssignmentOperatorCrystal.class),
    LEFT_LOGICAL_OR_ASSIGNMENT("\"<<", LeftOrAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_AND_ASSIGNMENT("^>>", RightAndAssignmentOperatorCrystal.class),
    RIGHT_LOGICAL_OR_ASSIGNMENT("\">>", RightOrAssignmentOperatorCrystal.class),

    ADD("+", AddOperatorCrystal.class),
    CONCATENATE("+", ConcatenateOperatorCrystal.class),
    LEFT_DIVIDE("/", LeftDivideOperatorCrystal.class),
    RIGHT_DIVIDE("\\", RightDivideOperatorCrystal.class),
    FEATHER("~", FeatherCrystal.class),
    DELETE("~", DeleteOperatorCrystal.class),
    LINE_CONTINUATION("~", LineContinuationOperatorCrystal.class),

    // logical and comparison operators
    // ^,",=,'=
    LOGICAL_AND("^", LogicalAndOperatorCrystal.class),
    LOGICAL_OR("\"", LogicalOrOperatorCrystal.class),
    LOGICAL_NOT("'", LogicalNotOperatorCrystal.class),
    EQUALS("=", EqualsComparisonOperatorCrystal.class),
    GREATER_THAN("<", GreaterThanComparisonOperatorCrystal.class),
    LESS_THAN(">", LessThanComparisonOperatorCrystal.class),
    GREATER_THAN_OR_EQUALS(">=", GreaterThanOrEqualsComparisonOperatorCrystal.class),
    LESS_THAN_OR_EQUALS("<=", LessThanOrEqualsComparisonOperatorCrystal.class),

    RETURN("^^", ReturnOperatorCrystal.class),
    KEY_VALUE_PAIR("=>", KeyValuePairOperatorCrystal.class),

    // Angel guards
    CATCH_ALL("||", CatchAllCrystal.class),
    LEFT_FEATHER_FALL_OPERATOR("\\\\", LeftFeatherFallCrystal.class),
    RIGHT_FEATHER_FALL_OPERATOR("//", RightFeatherFallCrystal.class),

    // Fear crystals
    FEAR_CRYSTAL("_.|._", FearCrystal.class),
    LEFT_FEAR_CRYSTAL("_.|.-",LeftFearCrystal.class),
    RIGHT_FEAR_CRYSTAL("-.|._", RightFearCrystal.class),
    LOVE_CRYSTAL("}*{", LoveCrystal.class),

    // two curriculums
    TEACHER_OF_TEACHERS("*~.", TeacherOfTeachersCrystal.class),
    STUDENT_OF_STUDENTS(".~*", StudentOfStudentsCrystal.class),

    ;

    DefaultIdentifierMapping(String identifier, Class<? extends AtonementCrystal> type) {
        this.identifier = identifier;
        this.type = type;
    }

    /**
     * The identifier's default string representation in Vikari.
     */
    final String identifier;

    /**
     * The class type for the identifer.
     */
    final Class<? extends AtonementCrystal> type;

    public String getIdentifier() {
        return identifier;
    }

    public Class<? extends AtonementCrystal> getType() {
        return type;
    }

    public static DefaultIdentifierMapping getMapping(String identifier) {
        for (DefaultIdentifierMapping mapping : DefaultIdentifierMapping.values()) {
            if (mapping.identifier.equals(identifier)) {
                return mapping;
            }
        }
        return null;
    }
}
