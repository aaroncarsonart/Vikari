package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;

/**
 * The add operator + adds the left and right operands.
 */
public class AddOperatorCrystal extends BinaryOperatorCrystal {

    public AddOperatorCrystal() {
        super(TokenType.ADD.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        return evaluateArithmeticOperator(Arithmetic::add, left, right);
    }

}
