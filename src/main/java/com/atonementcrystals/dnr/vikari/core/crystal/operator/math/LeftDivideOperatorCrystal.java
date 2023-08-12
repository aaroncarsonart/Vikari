package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;

/**
 * The left divide operator / divides the left operand by the right operand.
 */
public class LeftDivideOperatorCrystal extends BinaryOperatorCrystal {

    public LeftDivideOperatorCrystal() {
        super(TokenType.LEFT_DIVIDE.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        return evaluateArithmeticOperator(Arithmetic::divide, left, right);
    }

}
