package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;

/**
 * The multiply operator * multiplies the left operand by the right operand.<br/>
 * <br/>
 * A string or list may be multiplied by a number n to return a new string or list
 * which has repeated the original string or list n times.
 */
public class MultiplyOperatorCrystal extends BinaryOperatorCrystal {

    public MultiplyOperatorCrystal() {
        super(TokenType.MULTIPLY.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        return evaluateArithmeticOperator(Arithmetic::multiply, left, right);
    }

}
