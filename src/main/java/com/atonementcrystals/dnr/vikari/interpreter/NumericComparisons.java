package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.BiFunction;

public class NumericComparisons {

    /**
     * Compare the left and right operands. Performs type promotion such that the left or right operand are promoted to
     * the highest order type of the two operands. This allows numeric comparison of values regardless of their type.
     * @param left The left operand.
     * @param right The right operand.
     * @param compareIntegers The function to compare Integer operands.
     * @param compareLongs The function to compare Long operands.
     * @param compareBigIntegers The function to compare BigInteger operands.
     * @param compareFloats The function to compare Float operands.
     * @param compareDoubles The function to compare Double operands.
     * @param compareBigDecimals The function to compare BigDecimal operands.
     * @return The result of adding the left and right operands.
     */
    public static AtonementCrystal compare(NumberCrystal<?> left, NumberCrystal<?> right,
                                           BiFunction<Integer, Integer, Boolean> compareIntegers,
                                           BiFunction<Long, Long, Boolean> compareLongs,
                                           BiFunction<BigInteger, BigInteger, Boolean> compareBigIntegers,
                                           BiFunction<Float, Float, Boolean> compareFloats,
                                           BiFunction<Double, Double, Boolean> compareDoubles,
                                           BiFunction<BigDecimal, BigDecimal, Boolean> compareBigDecimals) {
        // ------------------------------------
        // 1. Both operands are integral types.
        // ------------------------------------
        if (left instanceof IntegerCrystal && right instanceof IntegerCrystal) {
            Integer leftValue = ((IntegerCrystal) left).getValue();
            Integer rightValue = ((IntegerCrystal) right).getValue();

            Boolean result = compareIntegers.apply(leftValue, rightValue);
            BooleanCrystal resultCrystal = new BooleanCrystal(result);
            return resultCrystal;
        }
        if (left instanceof LongCrystal && right instanceof LongCrystal) {
            Long leftValue = ((LongCrystal) left).getValue();
            Long rightValue = ((LongCrystal) right).getValue();

            Boolean result = compareLongs.apply(leftValue, rightValue);
            BooleanCrystal resultCrystal = new BooleanCrystal(result);
            return resultCrystal;
        }
        if (left instanceof BigIntegerCrystal && right instanceof BigIntegerCrystal) {
            BigInteger leftValue = ((BigIntegerCrystal) left).getValue();
            BigInteger rightValue = ((BigIntegerCrystal) right).getValue();

            Boolean result = compareBigIntegers.apply(leftValue, rightValue);
            BooleanCrystal resultCrystal = new BooleanCrystal(result);
            return resultCrystal;
        }
        // -----------------------------------
        // 2. Both operands are decimal types.
        // -----------------------------------
        if (left instanceof FloatCrystal && right instanceof FloatCrystal) {
            Float leftValue = ((FloatCrystal) left).getValue();
            Float rightValue = ((FloatCrystal) right).getValue();

            Boolean result = compareFloats.apply(leftValue, rightValue);
            BooleanCrystal resultCrystal = new BooleanCrystal(result);
            return resultCrystal;
        }
        if (left instanceof DoubleCrystal && right instanceof DoubleCrystal) {
            Double leftValue = ((DoubleCrystal) left).getValue();
            Double rightValue = ((DoubleCrystal) right).getValue();

            Boolean result = compareDoubles.apply(leftValue, rightValue);
            BooleanCrystal resultCrystal = new BooleanCrystal(result);
            return resultCrystal;

        }
        if (left instanceof BigDecimalCrystal && right instanceof BigDecimalCrystal) {
            BigDecimal leftValue = ((BigDecimalCrystal) left).getValue();
            BigDecimal rightValue = ((BigDecimalCrystal) right).getValue();

            Boolean result = compareBigDecimals.apply(leftValue, rightValue);
            BooleanCrystal resultCrystal = new BooleanCrystal(result);
            return resultCrystal;
        }
        // -------------------------------
        // 3. Operands are a mix of types.
        // -------------------------------
        // Upcast to BigDecimal
        if (left instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(right.getValue().toString(), Arithmetic.getMathContext());
            BigDecimalCrystal right2 = new BigDecimalCrystal(right.getIdentifier(), value);
            return compare(left, right2, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        } else if (right instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(left.getValue().toString(), Arithmetic.getMathContext());
            BigDecimalCrystal left2 = new BigDecimalCrystal(left.getIdentifier(), value);
            return compare(left2, right, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        }
        // Upcast to Double
        if (left instanceof DoubleCrystal) {
            DoubleCrystal right2 = new DoubleCrystal(right.getIdentifier(), right.getValue().toString());
            return compare(left, right2, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        } else if (right instanceof DoubleCrystal) {
            DoubleCrystal left2 = new DoubleCrystal(left.getIdentifier(), left.getValue().toString());
            return compare(left2, right, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        }
        // Upcast to Float
        if (left instanceof FloatCrystal) {
            FloatCrystal right2 = new FloatCrystal(right.getIdentifier(), right.getValue().toString());
            return compare(left, right2, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        } else if (right instanceof FloatCrystal) {
            FloatCrystal left2 = new FloatCrystal(left.getIdentifier(), left.getValue().toString());
            return compare(left2, right, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        }
        // Upcast to BigInteger
        if (left instanceof BigIntegerCrystal) {
            BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), right.getValue().toString());
            return compare(left, right2, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        } else if (right instanceof BigIntegerCrystal) {
            BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), left.getValue().toString());
            return compare(left2, right, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        }
        // Upcast to Long
        if (left instanceof LongCrystal) {
            LongCrystal right2 = new LongCrystal(right.getIdentifier(), right.getValue().toString());
            return compare(left, right2, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        } else if (right instanceof LongCrystal) {
            LongCrystal left2 = new LongCrystal(left.getIdentifier(), left.getValue().toString());
            return compare(left2, right, compareIntegers, compareLongs, compareBigIntegers, compareFloats,
                    compareDoubles, compareBigDecimals);
        }
        throw new IllegalStateException("Unreachable code.");
    }

    public static AtonementCrystal isEqual(NumberCrystal<?> left, NumberCrystal<?> right) {
        return compare(left, right, Integer::equals, Long::equals, BigInteger::equals, Float::equals, Double::equals,
                (leftValue, rightValue) -> leftValue.compareTo(rightValue) == 0);
    }

    public static AtonementCrystal isNotEqual(NumberCrystal<?> left, NumberCrystal<?> right) {
        return compare(left, right,
                (leftValue, rightValue) -> !leftValue.equals(rightValue),
                (leftValue, rightValue) -> !leftValue.equals(rightValue),
                (leftValue, rightValue) -> !leftValue.equals(rightValue),
                (leftValue, rightValue) -> !leftValue.equals(rightValue),
                (leftValue, rightValue) -> !leftValue.equals(rightValue),
                (leftValue, rightValue) -> leftValue.compareTo(rightValue) != 0);
    }
}
