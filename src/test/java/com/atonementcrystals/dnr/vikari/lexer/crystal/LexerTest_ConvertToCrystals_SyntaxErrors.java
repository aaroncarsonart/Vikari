package com.atonementcrystals.dnr.vikari.lexer.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.comment.MultiLineCommentCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.MultiLineStringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.TypeLabelOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.WhitespaceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.quotation.BacktickQuotationCrystal;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * The syntax errors for these cases have already been tested in the individual
 * LexerTest classes. So the following tests only validate the correct crystal
 * types are used for when the results are passed to the parser.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_ConvertToCrystals_SyntaxErrors {
    private static final CoordinatePair COORDINATE_PAIR_ZERO_ZERO = new CoordinatePair(0, 0);

    @Test
    @Order(1)
    public void testLexer_Crystals_SyntaxErrors_CommentPrefix() {
        String sourceString = "~:";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Error-case for a comment crystal has " +
                "incorrect type.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case comment crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal commentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "";
        String actualContents =  commentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case comment.");
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_SyntaxErrors_SingularBacktick() {
        String sourceString = "`";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(BacktickQuotationCrystal.class, crystal.getClass(), "Error-case for a singular backtick has " +
                "incorrect type.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case backtick should match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_SingleBacktickQuotation_IdentifierContainingNewline() {
        String sourceString = "`foo\n`:Integer << *";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error case for a backtick-quoted identifier has " +
                "incorrect type.");

        String expectedIdentifier = "`foo";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case backtick-quoted identifier should match its " +
                "source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // test statement 2
        statement = statementsOfCrystals.get(1);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error case for a backtick-quoted identifier has " +
                "incorrect type.");

        expectedIdentifier = "`:Integer << *";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case backtick-quoted identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_SingleBacktickQuotation_IdentifierMissingClosingBacktickQuote() {
        String sourceString = "foo:Integer << `bar";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 7;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        String expectedIdentifier = "foo";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(TypeLabelOperatorCrystal.class, crystal.getClass(), "Type label operator has incorrect type.");

        expectedIdentifier = ":";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A type label operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 3);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        expectedIdentifier = "Integer";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        expectedCoordinates = new CoordinatePair(0, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 11);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 12);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // sixth crystal
        crystal = statement.get(5);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 14);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // seventh crystal
        crystal = statement.get(6);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`bar";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 15);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_SingleBacktickQuotation_ContainingTabsShouldFail() {
        String sourceString = "`foo\tbar`:Integer << 2";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 7;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        String expectedIdentifier = "`foo\tbar`";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(TypeLabelOperatorCrystal.class, crystal.getClass(), "Type label operator has incorrect type.");

        expectedIdentifier = ":";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A type label operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 9);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        expectedIdentifier = "Integer";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        expectedCoordinates = new CoordinatePair(0, 10);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 17);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 18);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // sixth crystal
        crystal = statement.get(5);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 20);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // seventh crystal
        crystal = statement.get(6);
        assertEquals(IntegerCrystal.class, crystal.getClass(), "Integer literal has incorrect type.");

        expectedIdentifier = "2";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An integer literal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 21);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_SingleBacktickQuotation_OnlyWhitespaceCharactersShouldFail() {
        // ------------
        // single space
        // ------------
        String sourceString = "space:Character << ` `";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 7;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        String expectedIdentifier = "space";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(TypeLabelOperatorCrystal.class, crystal.getClass(), "Type label operator has incorrect type.");

        expectedIdentifier = ":";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A type label operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        expectedIdentifier = "Character";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        expectedCoordinates = new CoordinatePair(0, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 15);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 16);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // sixth crystal
        crystal = statement.get(5);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 18);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // seventh crystal
        crystal = statement.get(6);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Character literal has incorrect type.");

        expectedIdentifier = "` `";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A character literal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 19);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // ---------------
        // multiple spaces
        // ---------------
        sourceString = "`   ` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        expectedStatementsCount = 1;
        actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        statement = statementsOfCrystals.get(0);

        expectedSize = 5;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`   `";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 8);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(MultiplyOperatorCrystal.class, crystal.getClass(), "Constructor operator has incorrect type.");

        expectedIdentifier = "*";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A constructor operator's identifier should match its" +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 9);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // ----------
        // single tab
        // ----------
        sourceString = "`\t` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        expectedStatementsCount = 1;
        actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        statement = statementsOfCrystals.get(0);

        expectedSize = 5;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`\t`";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 3);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(MultiplyOperatorCrystal.class, crystal.getClass(), "Constructor operator has incorrect type.");

        expectedIdentifier = "*";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A constructor operator's identifier should match its" +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 7);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // -------------
        // multiple tabs
        // -------------
        sourceString = "`\t\t` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        expectedStatementsCount = 1;
        actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        statement = statementsOfCrystals.get(0);

        expectedSize = 5;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`\t\t`";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 7);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(MultiplyOperatorCrystal.class, crystal.getClass(), "Constructor operator has incorrect type.");

        expectedIdentifier = "*";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A constructor operator's identifier should match its" +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 8);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // ----------------------
        // mix of spaces and tabs
        // ----------------------
        sourceString = "` \t  \t\t   ` << *";

        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        expectedStatementsCount = 1;
        actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        statement = statementsOfCrystals.get(0);

        expectedSize = 5;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "` \t  \t\t   `";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 11);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 12);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 14);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(MultiplyOperatorCrystal.class, crystal.getClass(), "Constructor operator has incorrect type.");

        expectedIdentifier = "*";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A constructor operator's identifier should match its" +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 15);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_CommentEnclosure_ErrorHandlingForUnclosedComment_SingleLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Error-case comment crystal has incorrect " +
                "type.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case comment crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal commentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "`a` is approximately: [pi * 100].";
        String actualContents =  commentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case comment.");
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_CommentEnclosure_ErrorHandlingForUnclosedComment_MultiLine() {
        String sourceString = "~:`a` is approximately: [pi * 100].\n" +
                              "However, I forgot to close this comment!";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Error-case comment crystal has incorrect " +
                "type.");

        String expectedIdentifier = "~:`a` is approximately: [pi * 100].";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case comment crystal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal firstCommentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "`a` is approximately: [pi * 100].";
        String actualContents =  firstCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case comment.");

        // test statement 2
        statement = statementsOfCrystals.get(1);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Error-case comment crystal has incorrect " +
                "type.");

        expectedIdentifier = "However, I forgot to close this comment!";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal secondCommentCrystal = (MultiLineCommentCrystal) crystal;
        expectedContents = "However, I forgot to close this comment!";
        actualContents =  secondCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case comment.");

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
    @Order(9)
    public void testLexer_Crystals_CaptureQuotations_ErrorHandlingForUnclosedString_SingleLine() {
        String sourceString = "``This is a malformed string literal.";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Error-case string literal has " +
                "incorrect type.");

        String expectedIdentifier = sourceString;
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case string literal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal stringCrystal = (MultiLineStringLiteralCrystal) crystal;
        String expectedContents = "This is a malformed string literal.";
        String actualContents =  stringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case string literal.");
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_CaptureQuotations_ErrorHandlingForUnclosedString_MultiLine() {
        String sourceString = "``This is a malformed string literal \n" +
                              "because it has no ending capture quotation!";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Error-case string literal has " +
                "incorrect type.");

        String expectedIdentifier = "``This is a malformed string literal ";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case string literal's identifier should match " +
                "its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal firstStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        String expectedContents = "This is a malformed string literal ";
        String actualContents =  firstStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case string literal.");

        // test statement 2
        statement = statementsOfCrystals.get(1);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Error-case string literal has " +
                "incorrect type.");

        expectedIdentifier = "because it has no ending capture quotation!";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case string literal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal secondStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "because it has no ending capture quotation!";
        actualContents =  secondStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case string literal.");

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
    @Order(11)
    public void lexerTest_Crystals_SyntaxErrorCombos_multipleBacktickQuotations_andCaptureQuotation() {
        String sourceString = "a << `foo\n" +
                              "`z\tz` << a * 2\n" +
                              "bar:String << `  `\n" +
                              ":``baz``:``buzz:";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 4;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 5;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        String expectedIdentifier = "a";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 1);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 2);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`foo";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // test statement 2
        statement = statementsOfCrystals.get(1);

        expectedSize = 9;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`z\tz`";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(1, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(1, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(1, 8);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        expectedIdentifier = "a";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        expectedCoordinates = new CoordinatePair(1, 9);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // sixth crystal
        crystal = statement.get(5);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(1, 10);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // seventh crystal
        crystal = statement.get(6);
        assertEquals(MultiplyOperatorCrystal.class, crystal.getClass(), "Multiply operator has incorrect type.");

        expectedIdentifier = "*";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(1, 11);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // eighth crystal
        crystal = statement.get(7);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(1, 12);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // ninth crystal
        crystal = statement.get(8);
        assertEquals(IntegerCrystal.class, crystal.getClass(), "Integer literal has incorrect type.");

        expectedIdentifier = "2";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An integer literal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(1, 13);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // test statement 3
        statement = statementsOfCrystals.get(2);

        expectedSize = 7;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        expectedIdentifier = "bar";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        expectedCoordinates = new CoordinatePair(2, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(TypeLabelOperatorCrystal.class, crystal.getClass(), "Type label operator has incorrect type.");

        expectedIdentifier = ":";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A type label operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(2, 3);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        expectedIdentifier = "String";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        expectedCoordinates = new CoordinatePair(2, 4);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(2, 10);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(2, 11);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // sixth crystal
        crystal = statement.get(5);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(2, 13);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // seventh crystal
        crystal = statement.get(6);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`  `";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "Error-case reference's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(2, 14);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // test statement 4
        statement = statementsOfCrystals.get(3);

        expectedSize = 4;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(TypeLabelOperatorCrystal.class, crystal.getClass(), "Print statement operator has incorrect " +
                "type.");

        expectedIdentifier = ":";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A print statement operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(3, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(StringLiteralCrystal.class, crystal.getClass(), "String literal has incorrect type.");

        expectedIdentifier = "``baz``";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A string literal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(3, 1);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        StringLiteralCrystal firstStringCrystal = (StringLiteralCrystal) crystal;
        String expectedContents = "baz";
        String actualContents = firstStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for string literal crystal.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(TypeLabelOperatorCrystal.class, crystal.getClass(), "Print statement operator has incorrect " +
                "type.");

        expectedIdentifier = ":";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A print statement operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(3, 8);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(MultiLineStringLiteralCrystal.class, crystal.getClass(), "Error-case string literal has " +
                "incorrect type.");

        expectedIdentifier = "``buzz:";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case string literal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(3, 9);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineStringLiteralCrystal secondStringCrystal = (MultiLineStringLiteralCrystal) crystal;
        expectedContents = "buzz:";
        actualContents = secondStringCrystal.getString();
        assertEquals(expectedContents, actualContents, "Unexpected contents for error-case string literal.");
    }

    @Test
    @Order(12)
    public void lexerTest_Crystals_SyntaxErrorCombos_multipleErrorsOnSameLine_andCommentSuffix() {
        String sourceString = "`z\tz` << `foo\n" +
                              "~:Unclosed comment.";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 5;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        String expectedIdentifier = "`z\tz`";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 8);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`foo";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 9);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // test statement 2
        statement = statementsOfCrystals.get(1);

        expectedSize = 1;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Error-case comment crystal has incorrect " +
                "type.");

        expectedIdentifier = "~:Unclosed comment.";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case comment crystal's identifier should match " +
                "its source string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal firstCommentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "Unclosed comment.";
        String actualContents = firstCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case comment.");
    }

    @Test
    @Order(13)
    public void lexerTest_Crystals_SyntaxErrors_tabIndentedCode() {
        String sourceString = "\t\t`z` << `foo\n" +
                              "\t\t~:Unclosed comment.";

        Lexer lexer = new Lexer();
        SyntaxErrorReporter errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        listOfStatementTokens = lexer.collapseTokens(listOfStatementTokens);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        int expectedStatementsCount = 2;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of statements in result.");

        // test statement 1
        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 6;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        AtonementCrystal crystal = statement.get(0);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        String expectedIdentifier = "\t\t";
        String lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        CoordinatePair expectedCoordinates = new CoordinatePair(0, 0);
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Reference has incorrect type.");

        expectedIdentifier = "`z`";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A reference's identifier should match its source string.");

        expectedCoordinates = new CoordinatePair(0, 2);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // third crystal
        crystal = statement.get(2);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 5);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fourth crystal
        crystal = statement.get(3);
        assertEquals(LeftAssignmentOperatorCrystal.class, crystal.getClass(), "Left assignment operator has " +
                "incorrect type.");

        expectedIdentifier = "<<";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A left assignment operator's identifier should match its " +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 6);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // fifth crystal
        crystal = statement.get(4);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = " ";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(0, 8);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // sixth crystal
        crystal = statement.get(5);
        assertEquals(ReferenceCrystal.class, crystal.getClass(), "Error-case reference has incorrect type.");

        expectedIdentifier = "`foo";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case reference's identifier should match its" +
                "source string.");

        expectedCoordinates = new CoordinatePair(0, 9);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // test statement 2
        statement = statementsOfCrystals.get(1);

        expectedSize = 2;
        actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        // first crystal
        crystal = statement.get(0);
        assertEquals(WhitespaceCrystal.class, crystal.getClass(), "Whitespace crystal has incorrect type.");

        expectedIdentifier = "\t\t";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "A whitespace crystal's identifier should match its source " +
                "string.");

        expectedCoordinates = new CoordinatePair(1, 0);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        // second crystal
        crystal = statement.get(1);
        assertEquals(MultiLineCommentCrystal.class, crystal.getClass(), "Error-case comment crystal has incorrect " +
                "type.");

        expectedIdentifier = "~:Unclosed comment.";
        lexedIdentifier = crystal.getIdentifier();
        assertEquals(expectedIdentifier, lexedIdentifier, "An error-case comment crystal's identifier should match" +
                "its source string.");

        expectedCoordinates = new CoordinatePair(1, 2);
        actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");

        MultiLineCommentCrystal firstCommentCrystal = (MultiLineCommentCrystal) crystal;
        String expectedContents = "Unclosed comment.";
        String actualContents = firstCommentCrystal.getComment();
        assertEquals(expectedContents, actualContents, "Unexpected contents for an error-case comment.");
    }
}
