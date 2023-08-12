package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;

/**
 * The right divide operator \ divides the right operand by the left operand.
 */
public class RightDivideOperatorCrystal extends BinaryOperatorCrystal {

    public RightDivideOperatorCrystal() {
        super(TokenType.RIGHT_DIVIDE.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        return evaluateArithmeticOperator(Arithmetic::divide, right, left);
    }

}
