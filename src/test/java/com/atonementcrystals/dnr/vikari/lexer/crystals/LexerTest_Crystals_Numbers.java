package com.atonementcrystals.dnr.vikari.lexer.crystals;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.*;
import static com.atonementcrystals.dnr.vikari.lexer.LexerTestUtils.lexSingleStatement;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class LexerTest_Crystals_Numbers {
    private MathContext mathContext = Arithmetic.getMathContext();

    private void lexAndTestNumberCrystal(String sourceString, Class<? extends AtonementCrystal> expectedClass,
                                         Object expectedValue) {
        List<AtonementCrystal> statement = lexSingleStatement(sourceString, 1);
        if (statement.get(0) instanceof NumberCrystal<?> numberCrystal) {
            testCrystal(numberCrystal, expectedClass, sourceString, location(0, 0));
            assertEquals(expectedValue, numberCrystal.getValue(), "Unexpected value for NumberCrystal.");
        } else {
            fail("Expected result to be of type NumberCrystal.");
        }
    }

    @Test
    @Order(1)
    public void testLexer_Crystals_Numbers_Integer() {
        lexAndTestNumberCrystal("5", IntegerCrystal.class, 5);
    }

    @Test
    @Order(2)
    public void testLexer_Crystals_Numbers_Long_LargeValue() {
        lexAndTestNumberCrystal("4000000000", LongCrystal.class, 4000000000L);
    }

    @Test
    @Order(3)
    public void testLexer_Crystals_Numbers_Long_WithSuffix1() {
        lexAndTestNumberCrystal("22L", LongCrystal.class, 22L);
    }

    @Test
    @Order(4)
    public void testLexer_Crystals_Numbers_Long_WithSuffix2() {
        lexAndTestNumberCrystal("47l", LongCrystal.class, 47L);
    }

    @Test
    @Order(5)
    public void testLexer_Crystals_Numbers_BigInteger_LargeValue() {
        BigInteger expectedValue = new BigInteger("10000000000000000000");
        lexAndTestNumberCrystal("10000000000000000000", BigIntegerCrystal.class, expectedValue);
    }

    @Test
    @Order(6)
    public void testLexer_Crystals_Numbers_BigInteger_WithSuffix1() {
        lexAndTestNumberCrystal("2B", BigIntegerCrystal.class, new BigInteger("2"));
    }

    @Test
    @Order(7)
    public void testLexer_Crystals_Numbers_BigInteger_WithSuffix2() {
        lexAndTestNumberCrystal("72b", BigIntegerCrystal.class, new BigInteger("72"));
    }

    @Test
    @Order(8)
    public void testLexer_Crystals_Numbers_Float_WithSuffix1() {
        lexAndTestNumberCrystal("5.0F", FloatCrystal.class, 5.0F);
    }

    @Test
    @Order(9)
    public void testLexer_Crystals_Numbers_Float_WithSuffix2() {
        lexAndTestNumberCrystal("6.28f", FloatCrystal.class, 6.28F);
    }

    @Test
    @Order(10)
    public void testLexer_Crystals_Numbers_Double() {
        lexAndTestNumberCrystal("5.0", DoubleCrystal.class, 5.0);
    }

    @Test
    @Order(11)
    public void testLexer_Crystals_Numbers_Double_WithSuffix1() {
        lexAndTestNumberCrystal("5.0D", DoubleCrystal.class, 5.0);
    }

    @Test
    @Order(12)
    public void testLexer_Crystals_Numbers_Double_WithSuffix2() {
        lexAndTestNumberCrystal("3.14d", DoubleCrystal.class, 3.14);
    }

    @Test
    @Order(13)
    public void testLexer_Crystals_Numbers_BigDecimal_WithSuffix1() {
        lexAndTestNumberCrystal("5.0B", BigDecimalCrystal.class, new BigDecimal("5.0", mathContext));
    }

    @Test
    @Order(14)
    public void testLexer_Crystals_Numbers_BigDecimal_WithSuffix2() {
        lexAndTestNumberCrystal("3.14b", BigDecimalCrystal.class, new BigDecimal("3.14", mathContext));
    }
}
