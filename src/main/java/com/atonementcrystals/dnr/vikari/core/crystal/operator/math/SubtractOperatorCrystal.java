package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;

/**
 * The subtract operator - subtracts the right operand
 * from the left operand.
 */
public class SubtractOperatorCrystal extends BinaryOperatorCrystal {

    public SubtractOperatorCrystal() {
        super(TokenType.SUBTRACT.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        return evaluateArithmeticOperator(Arithmetic::subtract, left, right);
    }

}
