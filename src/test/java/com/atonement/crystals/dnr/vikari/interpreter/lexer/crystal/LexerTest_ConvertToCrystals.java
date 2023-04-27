package com.atonement.crystals.dnr.vikari.interpreter.lexer.crystal;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.comment.CommentCrystal;
import com.atonement.crystals.dnr.vikari.core.comment.MultiLineCommentCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.ReferenceCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;
import com.atonement.crystals.dnr.vikari.core.literal.BooleanLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.literal.MultiLineStringLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.literal.StringLiteralCrystal;
import com.atonement.crystals.dnr.vikari.core.literal.SwordCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.FunctionCallOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.AddOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.ModulusOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.MultiplyOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.math.SubtractCrystal;
import com.atonement.crystals.dnr.vikari.core.operator.prefix.DeleteOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.WhitespaceCrystal;
import com.atonement.crystals.dnr.vikari.core.separator.list.LeftParenthesisCrystal;
import com.atonement.crystals.dnr.vikari.error.SyntaxError;
import com.atonement.crystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonement.crystals.dnr.vikari.interpreter.Lexer;
import com.atonement.crystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.io.File;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_ConvertToCrystals {

    private static final CoordinatePair COORDINATE_PAIR_ZERO_ZERO = new CoordinatePair(0, 0);

    @Test
    @Order(1)
    public void lexerTest_BasicDefaultIdentifiers() {
        // Enclosures can't be individually lexed.
        Set<TokenType> enclosureTokenTypes = EnumSet.of(
                TokenType.COMMENT_PREFIX_CRYSTAL,
                TokenType.COMMENT_SUFFIX_CRYSTAL,
                TokenType.CAPTURE_QUOTATION,
                TokenType.BACKTICK);

        List<TokenType> tokenTypesToTest = TokenType.LEXER_TOKENS.stream()
                .filter(Predicate.not(enclosureTokenTypes::contains))
                .collect(Collectors.toCollection(ArrayList::new));

        for (TokenType tokenType : tokenTypesToTest) {
            String identifier = tokenType.getIdentifier();
            Class<? extends AtonementCrystal> clazz = tokenType.getType();

            String sourceString = identifier;

            Lexer lexer = new Lexer();
            List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
            listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
            List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

            int expectedStatementsCount = 1;
            int actualStatementsCount = statementsOfCrystals.size();
            assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

            List<AtonementCrystal> statement = statementsOfCrystals.get(0);

            int expectedSize = 1;
            int actualSize = statement.size();
            assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

            AtonementCrystal crystal = statement.get(0);
            assertEquals(clazz, crystal.getClass(), "Lexical analyzed crystal has incorrect type.");

            String lexedIdentifier = crystal.getIdentifier();
            assertEquals(identifier, lexedIdentifier, "Lexical analyzed crystal's identifier should match its input " +
                    "identifier.");

            CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
            CoordinatePair actualCoordinates = crystal.getCoordinates();
            assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
        }
    }

    /**
     * Basic sanity test for initial handling of assignment for a default value of an untyped identifier.
     */
    @Test
    @Order(2)
    public void lexerTest_Crystals_BasicAssignmentStatement() {
        String sourceString = "a << *";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 5;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        tokenNumber = 1;
        crystal = statement.get(tokenNumber);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        expectedCoordinates = new CoordinatePair(0, 1);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        tokenNumber = 2;
        crystal = statement.get(tokenNumber);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Unexpected crystal type for " +
                "token number " + tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source " +
                "string: \"" + sourceString + "\".");

        expectedCoordinates = new CoordinatePair(0, 2);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        tokenNumber = 3;
        crystal = statement.get(tokenNumber);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        expectedCoordinates = new CoordinatePair(0, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // NOTE: constructor crystals are initialized as multiply crystals at this step.
        tokenNumber = 4;
        crystal = statement.get(tokenNumber);
        assertEquals(MultiplyOperatorCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        expectedCoordinates = new CoordinatePair(0, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(3)
    public void lexerTest_Crystals_CommentPrefix() {
        String sourceString = "~:";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        lexer.convertTokensToCrystals(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for missing a closing comment prefix.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 1;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = sourceString;
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("comment suffix"), "Unexpected syntax error message.");
    }

    @Test
    @Order(4)
    public void lexerTest_Crystals_SingleLineComment() {
        String sourceString = "~:This is a comment.:~";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        assertEquals(CommentCrystal.class, crystal.getClass(), "Single-line comment crystal has incorrect type.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A single-line comment crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        CommentCrystal commentCrystal = (CommentCrystal) crystal;
        String expectedContents = "This is a comment.";
        String actualContents =  commentCrystal.getContents();
        assertEquals(expectedContents, actualContents, "Unexpected contents for single-line comment.");
    }

    @Test
    @Order(5)
    public void lexerTest_Crystals_TwoLineComment() {
        String sourceString = "~:This is a comment\n" +
                "across two lines.:~";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // Test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Multi-line comment crystal has incorrect type.");

        String expectedIdentifier = "~:This is a comment";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "The first part of this multi-line comment crystal's " +
                "identifier should match the first line of its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal firstCommentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "This is a comment";
        String actualContents = firstCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // Test statement 2
        statement = statementsOfCrystals.get(1);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        crystal = statement.get(0);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Multi-line comment crystal has incorrect " +
                "type.");

        expectedIdentifier = "across two lines.:~";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "The second part of this multi-line comment crystal's " +
                "identifier should match the second line of its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal secondCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "across two lines.";
        actualContents = secondCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // check linking of crystals
        MultiLineCommentCrystal expectedNext = secondCommentCrystal;
        MultiLineCommentCrystal actualNext = firstCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line comment crystal should be linked to the second " +
                "multi-line comment crystal.");

        expectedNext = null;
        actualNext = secondCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line comment crystal should be linked to null.");
    }

    @Test
    @Order(6)
    public void lexerTest_Crystals_ThreeLineComment() {
        String sourceString = "~:This is a comment\n" +
                "across three lines\n" +
                "without indentation.:~";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 3;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // first statement
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        Class<? extends AtonementCrystal> expectedClass = MultiLineCommentCrystal.class;
        Class<? extends AtonementCrystal> actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        String expectedIdentifier = "~:This is a comment";
        String actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal firstCommentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "This is a comment";
        String actualContents = firstCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // second statement
        statement = statementsOfCrystals.get(1);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        crystal = statement.get(0);
        expectedClass = MultiLineCommentCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        expectedIdentifier = "across three lines";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal secondCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "across three lines";
        actualContents = secondCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // third statement
        statement = statementsOfCrystals.get(2);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        crystal = statement.get(0);
        expectedClass = MultiLineCommentCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        expectedIdentifier = "without indentation.:~";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(2, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal thirdCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "without indentation.";
        actualContents = thirdCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // check linking of crystals
        MultiLineCommentCrystal expectedNext = secondCommentCrystal;
        MultiLineCommentCrystal actualNext = firstCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line comment crystal should be linked to the second " +
                "multi-line comment crystal.");

        expectedNext = thirdCommentCrystal;
        actualNext = secondCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line comment crystal should be linked to the third " +
                "multi-line comment crystal.");

        expectedNext = null;
        actualNext = thirdCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The third multi-likne comment comment crystal should not be linked" +
                "to another crystal.");
    }

    @Test
    @Order(7)
    public void lexerTest_Crystals_CommentCrystalsOnSeparateLines() {
        String sourceString = "~:\n" +
                "This is a multi-line comment\n" +
                "with the comment crystals on\n" +
                "separate lines from its contents.\n" +
                ":~";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 5;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // first statement
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        Class<? extends AtonementCrystal> expectedClass = MultiLineCommentCrystal.class;
        Class<? extends AtonementCrystal> actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        String expectedIdentifier = "~:";
        String actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal firstCommentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "";
        String actualContents = firstCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // second statement
        statement = statementsOfCrystals.get(1);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        crystal = statement.get(0);
        expectedClass = MultiLineCommentCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        expectedIdentifier = "This is a multi-line comment";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal secondCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "This is a multi-line comment";
        actualContents = secondCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // third statement
        statement = statementsOfCrystals.get(2);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        crystal = statement.get(0);
        expectedClass = MultiLineCommentCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        expectedIdentifier = "with the comment crystals on";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(2, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal thirdCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "with the comment crystals on";
        actualContents = thirdCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // fourth statement
        statement = statementsOfCrystals.get(3);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        crystal = statement.get(0);
        expectedClass = MultiLineCommentCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        expectedIdentifier = "separate lines from its contents.";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(3, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal fourthCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "separate lines from its contents.";
        actualContents = fourthCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // fifth statement
        statement = statementsOfCrystals.get(4);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        crystal = statement.get(0);
        expectedClass = MultiLineCommentCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line comment crystal has incorrect type.");

        expectedIdentifier = ":~";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(4, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal fifthCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "";
        actualContents = fifthCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line comment crystal.");

        // check linking of crystals
        MultiLineCommentCrystal expectedNext = secondCommentCrystal;
        MultiLineCommentCrystal actualNext = firstCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line comment crystal should be linked to the second " +
                "multi-line comment crystal.");

        expectedNext = thirdCommentCrystal;
        actualNext = secondCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line comment crystal should be linked to the third " +
                "multi-line comment crystal.");

        expectedNext = fourthCommentCrystal;
        actualNext = thirdCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The third multi-line comment crystal should be linked to the fourth " +
                "multi-line comment crystal.");

        expectedNext = fifthCommentCrystal;
        actualNext = fourthCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The fourth multi-line comment crystal should be linked to the fifth " +
                "multi-line comment crystal.");

        expectedNext = null;
        actualNext = fifthCommentCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The fifth multi-line comment crystal should not be linked to another " +
                "crystal.");
    }

    @Test
    @Order(8)
    public void lexerTest_Crystals_SingleLineStringLiteral() {
        String sourceString = "``This is a string literal.``";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        assertEquals(StringLiteralCrystal.class, crystal.getClass(), "Single-line string literal crystal has " +
                "incorrect type.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A single-line string literal crystal's identifier should " +
                "match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(9)
    public void lexerTest_Crystals_TwoLineStringLiteral() {
        String sourceString = "``This is a string literal\n" +
                "across two lines.``";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Multi-line string literal crystal has " +
                "incorrect type.");

        String expectedIdentifier = "``This is a string literal";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal firstStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        String expectedContents = "This is a string literal";
        String actualContents = firstStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // statement 2
        statement = statementsOfCrystals.get(1);

        crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Multi-line string literal crystal has " +
                "incorrect type.");

        expectedIdentifier = "across two lines.``";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal secondStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "across two lines.";
        actualContents = secondStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // check linking of crystals
        MultiLineStringLiteralCrystal expectedNext = secondStringCrystal;
        MultiLineStringLiteralCrystal actualNext = firstStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line string literal crystal should be linked to the " +
                "second multi-line string literal crystal.");

        expectedNext = null;
        actualNext = secondStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line string literal crystal should be linked to " +
                "null.");
    }

    @Test
    @Order(10)
    public void lexerTest_Crystals_ThreeLineStringLiteral() {
        String sourceString = "``This is a string literal\n" +
                "across three lines\n" +
                "without indentation.``";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 3;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Multi-line string literal crystal has " +
                "incorrect type.");

        String expectedIdentifier = "``This is a string literal";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal firstStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        String expectedContents = "This is a string literal";
        String actualContents = firstStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // statement 2
        statement = statementsOfCrystals.get(1);

        crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Multi-line string literal crystal has " +
                "incorrect type.");

        expectedIdentifier = "across three lines";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal secondStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "across three lines";
        actualContents = secondStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // statement 3
        statement = statementsOfCrystals.get(2);

        crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Multi-line string literal crystal has " +
                "incorrect type.");

        expectedIdentifier = "without indentation.``";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        expectedCoordinates = new CoordinatePair(2, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal thirdStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "without indentation.";
        actualContents = thirdStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // check linking of crystals
        MultiLineStringLiteralCrystal expectedNext = secondStringCrystal;
        MultiLineStringLiteralCrystal actualNext = firstStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line string literal crystal should be linked to the " +
                "second multi-line string literal crystal.");

        expectedNext = thirdStringCrystal;
        actualNext = secondStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line string literal crystal should be linked to the " +
                "third multi-line string literal crystal.");

        expectedNext = null;
        actualNext = thirdStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The third multi-line string literal crystal should be linked to null.");
    }

    @Test
    @Order(11)
    public void lexerTest_Crystals_MultiLineStringLiteral_WithOtherCrystals_BeforeAndAfter() {
        String sourceString = "foo << bar!(``This is a string literal\n" +
                "across two lines.``|baz)";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // -----------
        // statement 1
        // -----------
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 8;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // reference
        AtonementCrystal crystal = statement.get(0);
        Class<? extends AtonementCrystal> expectedType = ReferenceCrystal.class;
        Class<? extends AtonementCrystal> actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Reference crystal has incorrect type.");

        String expectedIdentifier = "foo";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Reference crystal has incorrect identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // whitespace
        crystal = statement.get(1);
        expectedType = WhitespaceCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Whitespace crystal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 3);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // assignment operator
        crystal = statement.get(2);
        expectedType = LeftAssignmentOperatorCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Left assignment operator crystal has incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Left assignment operator crystal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // whitespace
        crystal = statement.get(3);
        expectedType = WhitespaceCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Whitespace crystal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // reference
        crystal = statement.get(4);
        expectedType = ReferenceCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Reference crystal has incorrect type.");

        expectedIdentifier = "bar";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Reference crystal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 7);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // function call operator
        crystal = statement.get(5);
        expectedType = FunctionCallOperatorCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Function call operator crystal has incorrect type.");

        expectedIdentifier = "!";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Function call operator crystal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 10);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // left parenthesis
        crystal = statement.get(6);
        expectedType = LeftParenthesisCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Left parenthesis crystal has incorrect type.");

        expectedIdentifier = "(";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Left parenthesis crystal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 11);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // multi-line string literal
        crystal = statement.get(7);
        expectedType = MultiLineStringLiteralCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "``This is a string literal";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Multi-line string literal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 12);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal firstStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        String expectedContents = "This is a string literal";
        String actualContents = firstStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // -----------
        // statement 2
        // -----------
        statement = statementsOfCrystals.get(1);

        // multi-line string literal
        crystal = statement.get(0);
        expectedType = MultiLineStringLiteralCrystal.class;
        actualType = crystal.getClass();
        assertEquals(expectedType, actualType, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "across two lines.``";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Multi-line string literal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal secondStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "across two lines.";
        actualContents = secondStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string literal crystal.");

        // check linking of crystals
        MultiLineStringLiteralCrystal expectedNext = secondStringCrystal;
        MultiLineStringLiteralCrystal actualNext = firstStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line string literal crystal should be linked to the" +
                "second multi-line string literal crystal.");

        expectedNext = null;
        actualNext = secondStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line string literal crystal should not be linked to " +
                "another crystal.");
    }

    @Test
    @Order(12)
    public void lexerTest_Crystals_CaptureQuotations_EnclosingCode_SingleLine() {
        String sourceString = "``a << *``";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // first statement
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        AtonementCrystal crystal = statement.get(0);
        Class<? extends AtonementCrystal> expectedClass = StringLiteralCrystal.class;
        Class<? extends AtonementCrystal> actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "String literal crystal has incorrect type.");

        String expectedIdentifier = "``a << *``";
        String actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A string literal crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        StringLiteralCrystal stringCrystal = (StringLiteralCrystal) crystal;
        String expectedContents = "a << *";
        String actualContents = stringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for string literal crystal.");
    }

    @Test
    @Order(13)
    public void lexerTest_Crystals_CaptureQuotations_EnclosingCode_OnSeparateLines_SingleLine() {
        String sourceString = "string << ``\n" +
                              "a << *\n" +
                              "`` + foo";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 3;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // ---------------
        // first statement
        // ---------------
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 5;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        Class<? extends AtonementCrystal> expectedClass = ReferenceCrystal.class;
        Class<? extends AtonementCrystal> actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Reference literal crystal has incorrect type.");

        String expectedIdentifier = "string";
        String actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A reference crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        expectedClass = WhitespaceCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "A whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A whitespace crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(0, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        expectedClass = LeftAssignmentOperatorCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Left assignment operator crystal has incorrect type.");

        expectedIdentifier = "<<";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "Left assignment operator crystal has incorrect identifier.");

        expectedCoordinates = new CoordinatePair(0, 7);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        expectedClass = WhitespaceCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "A whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A whitespace crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(0, 9);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        expectedClass = MultiLineStringLiteralCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "``";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(0, 10);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal firstStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        String expectedContents = "";
        String actualContents = firstStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // ----------------
        // second statement
        // ----------------
        statement = statementsOfCrystals.get(1);

        // first crystal
        crystal = statement.get(0);
        expectedClass = MultiLineStringLiteralCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "a << *";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal secondStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "a << *";
        actualContents = secondStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // ---------------
        // third statement
        // ---------------
        statement = statementsOfCrystals.get(2);

        // first crystal
        crystal = statement.get(0);
        expectedClass = MultiLineStringLiteralCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "``";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(2, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal thirdStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "";
        actualContents = thirdStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // second crystal
        crystal = statement.get(1);
        expectedClass = WhitespaceCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "A whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A whitespace crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(2, 2);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        expectedClass = AddOperatorCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Add operator crystal has incorrect type.");

        expectedIdentifier = "+";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "An add operator crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(2, 3);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        expectedClass = WhitespaceCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "A whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A whitespace crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(2, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        expectedClass = ReferenceCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "A reference crystal has incorrect type.");

        expectedIdentifier = "foo";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A reference crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(2, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // -------------------------
        // check linking of crystals
        // -------------------------
        MultiLineStringLiteralCrystal expectedNext = secondStringCrystal;
        MultiLineStringLiteralCrystal actualNext = firstStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line string literal crystal should be linked to the " +
                "second multi-line string literal crystal.");

        expectedNext = thirdStringCrystal;
        actualNext = secondStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line string literal crystal should be linked to the " +
                "third multi-line string literal crystal.");

        expectedNext = null;
        actualNext = thirdStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The third multi-line string literal crystal should be linked to null.");
    }

    @Test
    @Order(14)
    public void lexerTest_Crystals_CaptureQuotations_EnclosingCode_OnSeparateLines_MultipleLines() {
        String sourceString = "``\n" +
                "foo << 2 + [3 * -7]\n" +
                "bar << foo - [5 / 9.0]\n" +
                ":foo + bar\n" +
                "``";
        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 5;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // ---------------
        // first statement
        // ---------------
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        Class<? extends AtonementCrystal> expectedClass = MultiLineStringLiteralCrystal.class;
        Class<? extends AtonementCrystal> actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        String expectedIdentifier = "``";
        String actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal firstStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        String expectedContents = "";
        String actualContents = firstStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // ----------------
        // second statement
        // ----------------
        statement = statementsOfCrystals.get(1);

        // first crystal
        crystal = statement.get(0);
        expectedClass = MultiLineStringLiteralCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "foo << 2 + [3 * -7]";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal secondStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "foo << 2 + [3 * -7]";
        actualContents = secondStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // ---------------
        // third statement
        // ---------------
        statement = statementsOfCrystals.get(2);

        // first crystal
        crystal = statement.get(0);
        expectedClass = MultiLineStringLiteralCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "bar << foo - [5 / 9.0]";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        expectedCoordinates = new CoordinatePair(2, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal thirdStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "bar << foo - [5 / 9.0]";
        actualContents = thirdStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // ----------------
        // fourth statement
        // ----------------
        statement = statementsOfCrystals.get(3);

        // first crystal
        crystal = statement.get(0);
        expectedClass = MultiLineStringLiteralCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = ":foo + bar";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        expectedCoordinates = new CoordinatePair(3, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal fourthStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = ":foo + bar";
        actualContents = fourthStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // ---------------
        // fifth statement
        // ---------------
        statement = statementsOfCrystals.get(4);

        // first crystal
        crystal = statement.get(0);
        expectedClass = MultiLineStringLiteralCrystal.class;
        actualClass = crystal.getClass();
        assertEquals(expectedClass, actualClass, "Multi-line string literal crystal has incorrect type.");

        expectedIdentifier = "``";
        actualIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, actualIdentifier, "A multi-line string literal crystal's identifier should " +
                "match its source string.");

        expectedCoordinates = new CoordinatePair(4, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal fifthStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "";
        actualContents = fifthStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for multi-line string crystal.");

        // -------------------------
        // check linking of crystals
        // -------------------------
        MultiLineStringLiteralCrystal expectedNext = secondStringCrystal;
        MultiLineStringLiteralCrystal actualNext = firstStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The first multi-line string literal crystal should be linked to the " +
                "second multi-line string literal crystal.");

        expectedNext = thirdStringCrystal;
        actualNext = secondStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The second multi-line string literal crystal should be linked to the " +
                "third multi-line string literal crystal.");

        expectedNext = fourthStringCrystal;
        actualNext = thirdStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The third multi-line string literal crystal should be linked to the " +
                "fourth multi-line string literal crystal.");

        expectedNext = fifthStringCrystal;
        actualNext = fourthStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The fourth multi-line string literal crystal should be linked to the " +
                "fifth multi-line string literal crystal.");

        expectedNext = null;
        actualNext = fifthStringCrystal.getNext();
        assertEquals(expectedNext, actualNext, "The fifth string literal crystal should be linked to null.");
    }

    @Test
    @Order(15)
    public void lexerTest_Crystals_ErrorCase_SingularBacktick() {
        String sourceString = "`";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        lexer.convertTokensToCrystals(listOfStatementTokens);

        assertTrue(errorReporter.hasErrors(), "Expected a syntax error for missing a closing backtick.");

        List<SyntaxError> syntaxErrors = errorReporter.getSyntaxErrors();
        int expectedSize = 1;
        int actualSize = syntaxErrors.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of syntax errors.");

        // Syntax Error 1
        SyntaxError syntaxError = syntaxErrors.get(0);
        File expectedFile = null;
        File actualFile = syntaxError.getFile();
        assertEquals(expectedFile, actualFile, "Expected file to be null.");

        CoordinatePair expectedLocation = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualLocation = syntaxError.getLocation();
        assertEquals(expectedLocation, actualLocation, "Unexpected location.");

        String expectedLine = sourceString;
        String actualLine = syntaxError.getLine();
        assertEquals(expectedLine, actualLine, "Unexpected line.");

        String errorMessage = syntaxError.getMessage();
        assertTrue(errorMessage.contains("backtick"), "Unexpected syntax error message.");
    }

    @Test
    @Order(16)
    public void lexerTest_Crystals_TrueLiteral() {
        String sourceString = "true";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(BooleanLiteralCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        BooleanLiteralCrystal booleanCrystal = (BooleanLiteralCrystal) crystal;
        Boolean expectedValue = Boolean.TRUE;
        Boolean actualValue = booleanCrystal.getValue();
        assertEquals(expectedValue, actualValue, "Expected boolean literal to have value of true.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A boolean literal crystal's identifier should match its " +
                "source string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(17)
    public void lexerTest_Crystals_FalseLiteral() {
        String sourceString = "false";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(BooleanLiteralCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        BooleanLiteralCrystal booleanCrystal = (BooleanLiteralCrystal) crystal;
        Boolean expectedValue = Boolean.FALSE;
        Boolean actualValue = booleanCrystal.getValue();
        assertEquals(expectedValue, actualValue, "Expected boolean literal to have value of true.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A boolean literal crystal's identifier should match its " +
                "source string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Modulus and percent operators are handled together as modulus during the lexing phase.
     */
    @Test
    @Order(18)
    public void lexerTest_Crystals_Modulus() {
        String sourceString = "%";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(ModulusOperatorCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A modulus operator crystal's identifier should match its " +
                "source string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Multiply operator and constructor crystals are handled together as multiply during the lexing phase.
     */
    @Test
    @Order(19)
    public void lexerTest_Crystals_Multiply() {
        String sourceString = "*";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(MultiplyOperatorCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A multiply operator crystal's identifier should match its " +
                "source string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Subtract and negate operators are handled together as subtract during the lexing phase.
     */
    @Test
    @Order(20)
    public void lexerTest_Crystals_Subtract() {
        String sourceString = "-";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(SubtractCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A subtract crystal's identifier should match its source " +
                "string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Delete and line continuation operators are handled together as delete during the lexing phase.
     */
    @Test
    @Order(21)
    public void lexerTest_Crystals_Delete() {
        String sourceString = "~foo";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 2;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(DeleteOperatorCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        String expectedIdentifier = TokenType.DELETE.getIdentifier();
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A subtract crystal's identifier should match its source " +
                "string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Add and concatenate operators are handled together as add during the lexing phase.
     */
    @Test
    @Order(22)
    public void lexerTest_Crystals_Add() {
        String sourceString = "+";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(AddOperatorCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An add crystal's identifier should match its source " +
                "string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Add and concatenate operators are handled together as add during the lexing phase.
     */
    @Test
    @Order(23)
    public void lexerTest_Crystals_SwordOfLengthOne() {
        String sourceString = "_";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(SwordCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        SwordCrystal swordCrystal = (SwordCrystal) crystal;
        int expectedLength = 1;
        int actualLength = swordCrystal.getLength();
        assertEquals(expectedLength, actualLength, "Unexpected sword length.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A subtract crystal's identifier should match its source " +
                "string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Add and concatenate operators are handled together as add during the lexing phase.
     */
    @Test
    @Order(24)
    public void lexerTest_Crystals_SwordOfLengthTwo() {
        String sourceString = "__";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(SwordCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        SwordCrystal swordCrystal = (SwordCrystal) crystal;
        int expectedLength = 2;
        int actualLength = swordCrystal.getLength();
        assertEquals(expectedLength, actualLength, "Unexpected sword length.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A subtract crystal's identifier should match its source " +
                "string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    /**
     * Add and concatenate operators are handled together as add during the lexing phase.
     */
    @Test
    @Order(25)
    public void lexerTest_Crystals_SwordOfLengthThree() {
        String sourceString = "___";

        Lexer lexer = new Lexer();
        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(SwordCrystal.class, crystal.getClass(), "Unexpected crystal type for token number " +
                tokenNumber + " with identifier \"" + crystal.getIdentifier() +  "\" in source string: \"" +
                sourceString + "\".");

        SwordCrystal swordCrystal = (SwordCrystal) crystal;
        int expectedLength = 3;
        int actualLength = swordCrystal.getLength();
        assertEquals(expectedLength, actualLength, "Unexpected sword length.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A subtract crystal's identifier should match its source " +
                "string's identifier.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }
}
