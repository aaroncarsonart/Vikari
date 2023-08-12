package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.interpreter.ArithmeticFunction;

public abstract class BinaryOperatorCrystal extends AtonementCrystal {

    public BinaryOperatorCrystal(String identifier) {
        super(identifier);
    }

    public abstract AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right);

    public AtonementCrystal evaluateArithmeticOperator(ArithmeticFunction arithmeticFunction, AtonementCrystal left,
                                                       AtonementCrystal right) {
        AtonementCrystal result = null;
        if (left instanceof NumberCrystal<?> leftNumber && right instanceof NumberCrystal<?> rightNumber) {
            result = arithmeticFunction.apply(leftNumber, rightNumber);
        }
        return result;
    }
}
