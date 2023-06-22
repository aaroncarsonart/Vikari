package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static org.junit.jupiter.api.Assertions.assertEquals;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_Numbers {
    private static final CoordinatePair COORDINATE_PAIR_ZERO_ZERO = new CoordinatePair(0, 0);

    @Test
    @Order(1)
    public void testLexer_Crystals_Numbers_Integer() {
        String sourceString = "5";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(IntegerCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        IntegerCrystal number = (IntegerCrystal) crystal;
        int expectedValue = 5;
        int actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_Numbers_Long_LargeValue() {
        String sourceString = "4000000000";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(LongCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        LongCrystal number = (LongCrystal) crystal;
        long expectedValue = 4_000_000_000L;
        long actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_Numbers_Long_WithSuffix1() {
        String sourceString = "22L";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(LongCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        LongCrystal number = (LongCrystal) crystal;
        long expectedValue = 22L;
        long actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_Numbers_Long_WithSuffix2() {
        String sourceString = "47l";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(LongCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        LongCrystal number = (LongCrystal) crystal;
        long expectedValue = 47l;
        long actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_Numbers_BigInteger_LargeValue() {
        String sourceString = "10000000000000000000";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(BigIntegerCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        BigIntegerCrystal number = (BigIntegerCrystal) crystal;
        BigInteger expectedValue = new BigInteger(sourceString);
        BigInteger actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_Numbers_BigInteger_WithSuffix1() {
        String sourceString = "2B";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(BigIntegerCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        BigIntegerCrystal number = (BigIntegerCrystal) crystal;
        BigInteger expectedValue = new BigInteger("2");
        BigInteger actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_Numbers_BigInteger_WithSuffix2() {
        String sourceString = "72b";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(BigIntegerCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        BigIntegerCrystal number = (BigIntegerCrystal) crystal;
        BigInteger expectedValue = new BigInteger("72");
        BigInteger actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_Numbers_Float_WithSuffix1() {
        String sourceString = "5.0F";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(FloatCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        FloatCrystal number = (FloatCrystal) crystal;
        float expectedValue = 5.0F;
        float actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_Numbers_Float_WithSuffix2() {
        String sourceString = "6.28f";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(FloatCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        FloatCrystal number = (FloatCrystal) crystal;
        float expectedValue = 6.28f;
        float actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_Numbers_Double() {
        String sourceString = "5.0";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(DoubleCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        DoubleCrystal number = (DoubleCrystal) crystal;
        double expectedValue = 5.0;
        double actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_Numbers_Double_WithSuffix1() {
        String sourceString = "5.0D";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(DoubleCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        DoubleCrystal number = (DoubleCrystal) crystal;
        double expectedValue = 5.0D;
        double actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(12)
    public void testLexer_Crystals_Numbers_Double_WithSuffix2() {
        String sourceString = "3.14d";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(DoubleCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        DoubleCrystal number = (DoubleCrystal) crystal;
        double expectedValue = 3.14d;
        double actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(13)
    public void testLexer_Crystals_Numbers_BigDecimal_WithSuffix1() {
        String sourceString = "5.0B";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(BigDecimalCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        BigDecimalCrystal number = (BigDecimalCrystal) crystal;
        BigDecimal expectedValue = new BigDecimal("5.0", Arithmetic.getMathContext());
        BigDecimal actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }

    @Test
    @Order(14)
    public void testLexer_Crystals_Numbers_BigDecimal_WithSuffix2() {
        String sourceString = "3.14b";

        Lexer lexer = new Lexer();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<String>> listOfStatementTokens = lexer.lexToStringTokens(sourceString);
        List<List<AtonementCrystal>> statementsOfCrystals = lexer.convertTokensToCrystals(listOfStatementTokens);

        assertNoSyntaxErrors(syntaxErrorReporter);

        int expectedStatementsCount = 1;
        int actualStatementsCount = statementsOfCrystals.size();
        assertEquals(expectedStatementsCount, actualStatementsCount, "Unexpected number of crystals in statement.");

        List<AtonementCrystal> statement = statementsOfCrystals.get(0);

        int expectedSize = 1;
        int actualSize = statement.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of crystals in statement.");

        int tokenNumber = 0;
        AtonementCrystal crystal = statement.get(tokenNumber);
        assertEquals(BigDecimalCrystal.class, crystal.getClass(), "Unexpected crystal type.");

        BigDecimalCrystal number = (BigDecimalCrystal) crystal;
        BigDecimal expectedValue = new BigDecimal("3.14", Arithmetic.getMathContext());
        BigDecimal actualValue = number.getValue();
        assertEquals(expectedValue, actualValue, "Unexpected value for number.");

        CoordinatePair expectedCoordinates = COORDINATE_PAIR_ZERO_ZERO;
        CoordinatePair actualCoordinates = crystal.getCoordinates();
        assertEquals(expectedCoordinates, actualCoordinates, "Unexpected coordinates.");
    }
}
