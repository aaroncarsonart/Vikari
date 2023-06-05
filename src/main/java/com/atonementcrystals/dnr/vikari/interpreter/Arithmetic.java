package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

public class Arithmetic {
    private static final MathContext DEFAULT_MATH_CONTEXT = MathContext.DECIMAL128;
    private static MathContext mathContext = DEFAULT_MATH_CONTEXT;
    private static int precision = DEFAULT_MATH_CONTEXT.getPrecision();
    private static RoundingMode roundingMode = DEFAULT_MATH_CONTEXT.getRoundingMode();

    public static void setBigDecimalPrecision(int precision) {
        Arithmetic.precision = precision;
        mathContext = new MathContext(precision, roundingMode);
    }

    public static void setBigDecimalRoundingMode(RoundingMode roundingMode) {
        Arithmetic.roundingMode = roundingMode;
        mathContext = new MathContext(precision, roundingMode);
    }

    public static MathContext getMathContext() {
        return mathContext;
    }

    /**
     * Add the left and right operands. Peforms type promotion of integers or longs if the result
     * would overflow or underflow beyond the maximum and minimum values of said types, respectively.
     * @param left The left operand.
     * @param right The right operand.
     * @return The result of adding the left and right operands.
     */
    public static AtonementCrystal add(NumberCrystal left, NumberCrystal right) {
        // ------------------------------------
        // 1. Both operands are integral types.
        // ------------------------------------
        if (left instanceof IntegerCrystal && right instanceof IntegerCrystal) {
            int leftValue = ((IntegerCrystal) left).getValue();
            int rightValue = ((IntegerCrystal) right).getValue();
            try {
                int result = Math.addExact(leftValue, rightValue);
                IntegerCrystal resultCrystal = new IntegerCrystal(null, result);
                return resultCrystal;
            } catch (ArithmeticException e) {
                LongCrystal left2 = new LongCrystal(left.getIdentifier(), Long.valueOf(leftValue));
                LongCrystal right2 = new LongCrystal(right.getIdentifier(), Long.valueOf(rightValue));
                return add(left2, right2);
            }
        }
        if (left instanceof LongCrystal && right instanceof LongCrystal) {
            long leftValue = ((LongCrystal) left).getValue();
            long rightValue = ((LongCrystal) right).getValue();
            try {
                long result = Math.addExact(leftValue, rightValue);
                LongCrystal resultCrystal = new LongCrystal(null, result);
                return resultCrystal;
            } catch (ArithmeticException e) {
                BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), BigInteger.valueOf(leftValue));
                BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), BigInteger.valueOf(rightValue));
                return add(left2, right2);
            }
        }
        if (left instanceof BigIntegerCrystal && right instanceof BigIntegerCrystal) {
            BigInteger leftValue = ((BigIntegerCrystal) left).getValue();
            BigInteger rightValue = ((BigIntegerCrystal) right).getValue();
            BigInteger result = leftValue.add(rightValue);
            BigIntegerCrystal resultCrystal = new BigIntegerCrystal(null, result);
            return resultCrystal;
        }
        // -----------------------------------
        // 2. Both operands are decimal types.
        // -----------------------------------
        if (left instanceof FloatCrystal && right instanceof FloatCrystal) {
            float leftValue = ((FloatCrystal) left).getValue();
            float rightValue = ((FloatCrystal) right).getValue();

            float result = leftValue + rightValue;
            FloatCrystal resultCrystal = new FloatCrystal(null, result);
            return resultCrystal;
        }
        if (left instanceof DoubleCrystal && right instanceof DoubleCrystal) {
            double leftValue = ((DoubleCrystal) left).getValue();
            double rightValue = ((DoubleCrystal) right).getValue();

            double result = leftValue + rightValue;
            DoubleCrystal resultCrystal = new DoubleCrystal(null, result);
            return resultCrystal;

        }
        if (left instanceof BigDecimalCrystal && right instanceof BigDecimalCrystal) {
            BigDecimal leftValue = ((BigDecimalCrystal) left).getValue();
            BigDecimal rightValue = ((BigDecimalCrystal) right).getValue();

            BigDecimal result = leftValue.add(rightValue, mathContext);
            BigDecimalCrystal resultCrystal = new BigDecimalCrystal(null, result);
            return resultCrystal;
        }
        // -------------------------------
        // 3. Operands are a mix of types.
        // -------------------------------
        // Upcast to BigDecimal
        if (left instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(right.getValue().toString(), mathContext);
            BigDecimalCrystal right2 = new BigDecimalCrystal(right.getIdentifier(), value);
            return add(left, right2);
        } else if (right instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(left.getValue().toString(), mathContext);
            BigDecimalCrystal left2 = new BigDecimalCrystal(left.getIdentifier(), value);
            return add(left2, right);
        }
        // Upcast to Double
        if (left instanceof DoubleCrystal) {
            DoubleCrystal right2 = new DoubleCrystal(right.getIdentifier(), right.getValue().toString());
            return add(left, right2);
        } else if (right instanceof DoubleCrystal) {
            DoubleCrystal left2 = new DoubleCrystal(left.getIdentifier(), left.getValue().toString());
            return add(left2, right);
        }
        // Upcast to Float
        if (left instanceof FloatCrystal) {
            FloatCrystal right2 = new FloatCrystal(right.getIdentifier(), right.getValue().toString());
            return add(left, right2);
        } else if (right instanceof FloatCrystal) {
            FloatCrystal left2 = new FloatCrystal(left.getIdentifier(), left.getValue().toString());
            return add(left2, right);
        }
        // Upcast to BigInteger
        if (left instanceof BigIntegerCrystal) {
            BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), right.getValue().toString());
            return add(left, right2);
        } else if (right instanceof BigIntegerCrystal) {
            BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), left.getValue().toString());
            return add(left2, right);
        }
        // Upcast to Long
        if (left instanceof LongCrystal) {
            LongCrystal right2 = new LongCrystal(right.getIdentifier(), right.getValue().toString());
            return add(left, right2);
        } else if (right instanceof LongCrystal) {
            LongCrystal left2 = new LongCrystal(left.getIdentifier(), left.getValue().toString());
            return add(left2, right);
        }
        throw new IllegalStateException("Unreachable code.");
    }

    /**
     * Add the left and right operands. Peforms type promotion of integers or longs if the result
     * would overflow or underflow beyond the maximum and minimum values of said types, respectively.
     * @param left The left operand.
     * @param right The right operand.
     * @return The result of adding the left and right operands.
     */
    public static AtonementCrystal subtract(NumberCrystal left, NumberCrystal right) {
        // ------------------------------------
        // 1. Both operands are integral types.
        // ------------------------------------
        if (left instanceof IntegerCrystal && right instanceof IntegerCrystal) {
            int leftValue = ((IntegerCrystal) left).getValue();
            int rightValue = ((IntegerCrystal) right).getValue();
            try {
                int result = Math.subtractExact(leftValue, rightValue);
                IntegerCrystal resultCrystal = new IntegerCrystal(null, result);
                return resultCrystal;
            } catch (ArithmeticException e) {
                LongCrystal left2 = new LongCrystal(left.getIdentifier(), Long.valueOf(leftValue));
                LongCrystal right2 = new LongCrystal(right.getIdentifier(), Long.valueOf(rightValue));
                return subtract(left2, right2);
            }
        }
        if (left instanceof LongCrystal && right instanceof LongCrystal) {
            long leftValue = ((LongCrystal) left).getValue();
            long rightValue = ((LongCrystal) right).getValue();
            try {
                long result = Math.subtractExact(leftValue, rightValue);
                LongCrystal resultCrystal = new LongCrystal(null, result);
                return resultCrystal;
            } catch (ArithmeticException e) {
                BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), BigInteger.valueOf(leftValue));
                BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), BigInteger.valueOf(rightValue));
                return subtract(left2, right2);
            }
        }
        if (left instanceof BigIntegerCrystal && right instanceof BigIntegerCrystal) {
            BigInteger leftValue = ((BigIntegerCrystal) left).getValue();
            BigInteger rightValue = ((BigIntegerCrystal) right).getValue();
            BigInteger result = leftValue.subtract(rightValue);
            BigIntegerCrystal resultCrystal = new BigIntegerCrystal(null, result);
            return resultCrystal;
        }
        // -----------------------------------
        // 2. Both operands are decimal types.
        // -----------------------------------
        if (left instanceof FloatCrystal && right instanceof FloatCrystal) {
            float leftValue = ((FloatCrystal) left).getValue();
            float rightValue = ((FloatCrystal) right).getValue();

            float result = leftValue - rightValue;
            FloatCrystal resultCrystal = new FloatCrystal(null, result);
            return resultCrystal;
        }
        if (left instanceof DoubleCrystal && right instanceof DoubleCrystal) {
            double leftValue = ((DoubleCrystal) left).getValue();
            double rightValue = ((DoubleCrystal) right).getValue();

            double result = leftValue - rightValue;
            DoubleCrystal resultCrystal = new DoubleCrystal(null, result);
            return resultCrystal;

        }
        if (left instanceof BigDecimalCrystal && right instanceof BigDecimalCrystal) {
            BigDecimal leftValue = ((BigDecimalCrystal) left).getValue();
            BigDecimal rightValue = ((BigDecimalCrystal) right).getValue();

            BigDecimal result = leftValue.subtract(rightValue, mathContext);
            BigDecimalCrystal resultCrystal = new BigDecimalCrystal(null, result);
            return resultCrystal;
        }
        // -------------------------------
        // 3. Operands are a mix of types.
        // -------------------------------
        // Upcast to BigDecimal
        if (left instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(right.getValue().toString(), mathContext);
            BigDecimalCrystal right2 = new BigDecimalCrystal(right.getIdentifier(), value);
            return subtract(left, right2);
        } else if (right instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(left.getValue().toString(), mathContext);
            BigDecimalCrystal left2 = new BigDecimalCrystal(left.getIdentifier(), value);
            return subtract(left2, right);
        }
        // Upcast to Double
        if (left instanceof DoubleCrystal) {
            DoubleCrystal right2 = new DoubleCrystal(right.getIdentifier(), right.getValue().toString());
            return subtract(left, right2);
        } else if (right instanceof DoubleCrystal) {
            DoubleCrystal left2 = new DoubleCrystal(left.getIdentifier(), left.getValue().toString());
            return subtract(left2, right);
        }
        // Upcast to Float
        if (left instanceof FloatCrystal) {
            FloatCrystal right2 = new FloatCrystal(right.getIdentifier(), right.getValue().toString());
            return subtract(left, right2);
        } else if (right instanceof FloatCrystal) {
            FloatCrystal left2 = new FloatCrystal(left.getIdentifier(), left.getValue().toString());
            return subtract(left2, right);
        }
        // Upcast to BigInteger
        if (left instanceof BigIntegerCrystal) {
            BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), right.getValue().toString());
            return subtract(left, right2);
        } else if (right instanceof BigIntegerCrystal) {
            BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), left.getValue().toString());
            return subtract(left2, right);
        }
        // Upcast to Long
        if (left instanceof LongCrystal) {
            LongCrystal right2 = new LongCrystal(right.getIdentifier(), right.getValue().toString());
            return subtract(left, right2);
        } else if (right instanceof LongCrystal) {
            LongCrystal left2 = new LongCrystal(left.getIdentifier(), left.getValue().toString());
            return subtract(left2, right);
        }
        throw new IllegalStateException("Unreachable code.");
    }

    /**
     * Add the left and right operands. Peforms type promotion of integers or longs if the result
     * would overflow or underflow beyond the maximum and minimum values of said types, respectively.
     * @param left The left operand.
     * @param right The right operand.
     * @return The result of adding the left and right operands.
     */
    public static AtonementCrystal multiply(NumberCrystal left, NumberCrystal right) {
        // ------------------------------------
        // 1. Both operands are integral types.
        // ------------------------------------
        if (left instanceof IntegerCrystal && right instanceof IntegerCrystal) {
            int leftValue = ((IntegerCrystal) left).getValue();
            int rightValue = ((IntegerCrystal) right).getValue();
            try {
                int result = Math.multiplyExact(leftValue, rightValue);
                IntegerCrystal resultCrystal = new IntegerCrystal(null, result);
                return resultCrystal;
            } catch (ArithmeticException e) {
                LongCrystal left2 = new LongCrystal(left.getIdentifier(), Long.valueOf(leftValue));
                LongCrystal right2 = new LongCrystal(right.getIdentifier(), Long.valueOf(rightValue));
                return multiply(left2, right2);
            }
        }
        if (left instanceof LongCrystal && right instanceof LongCrystal) {
            long leftValue = ((LongCrystal) left).getValue();
            long rightValue = ((LongCrystal) right).getValue();
            try {
                long result = Math.multiplyExact(leftValue, rightValue);
                LongCrystal resultCrystal = new LongCrystal(null, result);
                return resultCrystal;
            } catch (ArithmeticException e) {
                BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), BigInteger.valueOf(leftValue));
                BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), BigInteger.valueOf(rightValue));
                return multiply(left2, right2);
            }
        }
        if (left instanceof BigIntegerCrystal && right instanceof BigIntegerCrystal) {
            BigInteger leftValue = ((BigIntegerCrystal) left).getValue();
            BigInteger rightValue = ((BigIntegerCrystal) right).getValue();
            BigInteger result = leftValue.multiply(rightValue);
            BigIntegerCrystal resultCrystal = new BigIntegerCrystal(null, result);
            return resultCrystal;
        }
        // -----------------------------------
        // 2. Both operands are decimal types.
        // -----------------------------------
        if (left instanceof FloatCrystal && right instanceof FloatCrystal) {
            float leftValue = ((FloatCrystal) left).getValue();
            float rightValue = ((FloatCrystal) right).getValue();

            float result = leftValue * rightValue;
            FloatCrystal resultCrystal = new FloatCrystal(null, result);
            return resultCrystal;
        }
        if (left instanceof DoubleCrystal && right instanceof DoubleCrystal) {
            double leftValue = ((DoubleCrystal) left).getValue();
            double rightValue = ((DoubleCrystal) right).getValue();

            double result = leftValue * rightValue;
            DoubleCrystal resultCrystal = new DoubleCrystal(null, result);
            return resultCrystal;

        }
        if (left instanceof BigDecimalCrystal && right instanceof BigDecimalCrystal) {
            BigDecimal leftValue = ((BigDecimalCrystal) left).getValue();
            BigDecimal rightValue = ((BigDecimalCrystal) right).getValue();

            BigDecimal result = leftValue.multiply(rightValue, mathContext);
            BigDecimalCrystal resultCrystal = new BigDecimalCrystal(null, result);
            return resultCrystal;
        }
        // -------------------------------
        // 3. Operands are a mix of types.
        // -------------------------------
        // Upcast to BigDecimal
        if (left instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(right.getValue().toString(), mathContext);
            BigDecimalCrystal right2 = new BigDecimalCrystal(right.getIdentifier(), value);
            return multiply(left, right2);
        } else if (right instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(left.getValue().toString(), mathContext);
            BigDecimalCrystal left2 = new BigDecimalCrystal(left.getIdentifier(), value);
            return multiply(left2, right);
        }
        // Upcast to Double
        if (left instanceof DoubleCrystal) {
            DoubleCrystal right2 = new DoubleCrystal(right.getIdentifier(), right.getValue().toString());
            return multiply(left, right2);
        } else if (right instanceof DoubleCrystal) {
            DoubleCrystal left2 = new DoubleCrystal(left.getIdentifier(), left.getValue().toString());
            return multiply(left2, right);
        }
        // Upcast to Float
        if (left instanceof FloatCrystal) {
            FloatCrystal right2 = new FloatCrystal(right.getIdentifier(), right.getValue().toString());
            return multiply(left, right2);
        } else if (right instanceof FloatCrystal) {
            FloatCrystal left2 = new FloatCrystal(left.getIdentifier(), left.getValue().toString());
            return multiply(left2, right);
        }
        // Upcast to BigInteger
        if (left instanceof BigIntegerCrystal) {
            BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), right.getValue().toString());
            return multiply(left, right2);
        } else if (right instanceof BigIntegerCrystal) {
            BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), left.getValue().toString());
            return multiply(left2, right);
        }
        // Upcast to Long
        if (left instanceof LongCrystal) {
            LongCrystal right2 = new LongCrystal(right.getIdentifier(), right.getValue().toString());
            return multiply(left, right2);
        } else if (right instanceof LongCrystal) {
            LongCrystal left2 = new LongCrystal(left.getIdentifier(), left.getValue().toString());
            return multiply(left2, right);
        }
        throw new IllegalStateException("Unreachable code.");
    }

    /**
     * Add the left and right operands. Peforms type promotion of integers or longs if the result
     * would overflow or underflow beyond the maximum and minimum values of said types, respectively.
     * @param left The left operand.
     * @param right The right operand.
     * @return The result of adding the left and right operands.
     */
    public static AtonementCrystal divide(NumberCrystal left, NumberCrystal right) {
        // ------------------------------------
        // 1. Both operands are integral types.
        // ------------------------------------
        if (left instanceof IntegerCrystal && right instanceof IntegerCrystal) {
            int leftValue = ((IntegerCrystal) left).getValue();
            int rightValue = ((IntegerCrystal) right).getValue();

            int result = leftValue / rightValue;
            IntegerCrystal resultCrystal = new IntegerCrystal(null, result);
            return resultCrystal;
        }
        if (left instanceof LongCrystal && right instanceof LongCrystal) {
            long leftValue = ((LongCrystal) left).getValue();
            long rightValue = ((LongCrystal) right).getValue();

            long result = leftValue / rightValue;
            LongCrystal resultCrystal = new LongCrystal(null, result);
            return resultCrystal;
        }
        if (left instanceof BigIntegerCrystal && right instanceof BigIntegerCrystal) {
            BigInteger leftValue = ((BigIntegerCrystal) left).getValue();
            BigInteger rightValue = ((BigIntegerCrystal) right).getValue();
            BigInteger result = leftValue.divide(rightValue);
            BigIntegerCrystal resultCrystal = new BigIntegerCrystal(null, result);
            return resultCrystal;
        }
        // -----------------------------------
        // 2. Both operands are decimal types.
        // -----------------------------------
        if (left instanceof FloatCrystal && right instanceof FloatCrystal) {
            float leftValue = ((FloatCrystal) left).getValue();
            float rightValue = ((FloatCrystal) right).getValue();

            float result = leftValue / rightValue;
            FloatCrystal resultCrystal = new FloatCrystal(null, result);
            return resultCrystal;
        }
        if (left instanceof DoubleCrystal && right instanceof DoubleCrystal) {
            double leftValue = ((DoubleCrystal) left).getValue();
            double rightValue = ((DoubleCrystal) right).getValue();

            double result = leftValue / rightValue;
            DoubleCrystal resultCrystal = new DoubleCrystal(null, result);
            return resultCrystal;

        }
        if (left instanceof BigDecimalCrystal && right instanceof BigDecimalCrystal) {
            BigDecimal leftValue = ((BigDecimalCrystal) left).getValue();
            BigDecimal rightValue = ((BigDecimalCrystal) right).getValue();

            BigDecimal result = leftValue.divide(rightValue, mathContext);
            BigDecimalCrystal resultCrystal = new BigDecimalCrystal(null, result);
            return resultCrystal;
        }
        // -------------------------------
        // 3. Operands are a mix of types.
        // -------------------------------
        // Upcast to BigDecimal
        if (left instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(right.getValue().toString(), mathContext);
            BigDecimalCrystal right2 = new BigDecimalCrystal(right.getIdentifier(), value);
            return divide(left, right2);
        } else if (right instanceof BigDecimalCrystal) {
            BigDecimal value = new BigDecimal(left.getValue().toString(), mathContext);
            BigDecimalCrystal left2 = new BigDecimalCrystal(left.getIdentifier(), value);
            return divide(left2, right);
        }
        // Upcast to Double
        if (left instanceof DoubleCrystal) {
            DoubleCrystal right2 = new DoubleCrystal(right.getIdentifier(), right.getValue().toString());
            return divide(left, right2);
        } else if (right instanceof DoubleCrystal) {
            DoubleCrystal left2 = new DoubleCrystal(left.getIdentifier(), left.getValue().toString());
            return divide(left2, right);
        }
        // Upcast to Float
        if (left instanceof FloatCrystal) {
            FloatCrystal right2 = new FloatCrystal(right.getIdentifier(), right.getValue().toString());
            return divide(left, right2);
        } else if (right instanceof FloatCrystal) {
            FloatCrystal left2 = new FloatCrystal(left.getIdentifier(), left.getValue().toString());
            return divide(left2, right);
        }
        // Upcast to BigInteger
        if (left instanceof BigIntegerCrystal) {
            BigIntegerCrystal right2 = new BigIntegerCrystal(right.getIdentifier(), right.getValue().toString());
            return divide(left, right2);
        } else if (right instanceof BigIntegerCrystal) {
            BigIntegerCrystal left2 = new BigIntegerCrystal(left.getIdentifier(), left.getValue().toString());
            return divide(left2, right);
        }
        // Upcast to Long
        if (left instanceof LongCrystal) {
            LongCrystal right2 = new LongCrystal(right.getIdentifier(), right.getValue().toString());
            return divide(left, right2);
        } else if (right instanceof LongCrystal) {
            LongCrystal left2 = new LongCrystal(left.getIdentifier(), left.getValue().toString());
            return divide(left2, right);
        }
        throw new IllegalStateException("Unreachable code.");
    }

    /**
     * Negate the NumberCrystal.
     * @param numberCrystal The NumberCrystal to negate.
     * @return The negated value of the NumberCrystal.
     */
    public static NumberCrystal negate(NumberCrystal numberCrystal) {
        String identifier = null;
        Object value = numberCrystal.getValue();
        NumberCrystal negatedCrystal = null;
        if (numberCrystal instanceof IntegerCrystal) {
            negatedCrystal = new IntegerCrystal(identifier, -(int) value);
        } else if (numberCrystal instanceof LongCrystal) {
            negatedCrystal = new LongCrystal(identifier, -(long) value);
        } else if (numberCrystal instanceof BigIntegerCrystal) {
            negatedCrystal = new BigIntegerCrystal(identifier, ((BigInteger) value).negate());
        } else if (numberCrystal instanceof FloatCrystal) {
            negatedCrystal = new FloatCrystal(identifier, -(float) value);
        } else if (numberCrystal instanceof DoubleCrystal) {
            negatedCrystal = new DoubleCrystal(identifier, -(double) value);
        } else if (numberCrystal instanceof BigDecimalCrystal) {
            negatedCrystal = new BigDecimalCrystal(identifier, ((BigDecimal) value).negate());
        }
        return negatedCrystal;
    }

    public static NumberCrystal maybeUpcastOrDowncast(NumberCrystal numberCrystal, TypeCrystal declaredType) {

        if (declaredType.isEqual(VikariType.ATONEMENT_CRYSTAL, VikariType.VALUE, VikariType.NUMBER)) {
            return numberCrystal;
        }

        TypeCrystal expressionType = numberCrystal.getInstantiatedType();

        if (declaredType == expressionType) {
            return numberCrystal;
        }

        Number number = (Number) numberCrystal.getValue();

        if (declaredType.isEqual(VikariType.INTEGER)) {
            int intValue;
            if (expressionType.isEqual(VikariType.FLOAT, VikariType.DOUBLE)) {
                long longValue = number.longValue();
                intValue = (int) longValue;
            } else {
                intValue = number.intValue();
            }
            return new IntegerCrystal(intValue);

        } else if (declaredType.isEqual(VikariType.LONG)) {
            long longValue;
            if (expressionType.isEqual(VikariType.FLOAT, VikariType.DOUBLE)) {
                BigInteger bigIntegerValue = BigDecimal.valueOf(number.doubleValue()).toBigInteger();
                longValue = bigIntegerValue.longValue();
            } else {
                longValue = number.longValue();
            }
            return new LongCrystal(longValue);

        } else if (declaredType.isEqual(VikariType.BIG_INTEGER)) {
            BigInteger bigIntegerValue;
            if (expressionType.isEqual(VikariType.FLOAT, VikariType.DOUBLE)) {
                String stringValue = number.toString();
                BigDecimal bigDecimalValue = new BigDecimal(stringValue, mathContext);
                bigIntegerValue = bigDecimalValue.toBigInteger();
            } else if (expressionType.isEqual(VikariType.BIG_DECIMAL)) {
                BigDecimal bigDecimalValue = ((BigDecimalCrystal) numberCrystal).getValue();
                bigIntegerValue = bigDecimalValue.toBigInteger();
            } else if (expressionType.isEqual(VikariType.BIG_INTEGER)) {
                bigIntegerValue = ((BigIntegerCrystal) numberCrystal).getValue();
            } else {
                // it is an integer or float type.
                long longValue = number.longValue();
                bigIntegerValue = BigInteger.valueOf(longValue);
            }
            return new BigIntegerCrystal(bigIntegerValue);

        } else if (declaredType.isEqual(VikariType.FLOAT)) {
            float floatValue = number.floatValue();
            return new FloatCrystal(floatValue);

        } else if (declaredType.isEqual(VikariType.DOUBLE)) {
            double doubleValue = number.doubleValue();
            return new DoubleCrystal(doubleValue);

        } else if (declaredType.isEqual(VikariType.BIG_DECIMAL)) {
            String stringValue = number.toString();
            BigDecimal bigDecimalValue = new BigDecimal(stringValue, mathContext);
            return new BigDecimalCrystal(bigDecimalValue);
        }

        throw new IllegalStateException("Unreachable code.");
    }
}
