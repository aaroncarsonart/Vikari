package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.UnaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;

/**
 * The unary negate operator - negates the right operand.
 */
public class NegateOperatorCrystal extends UnaryOperatorCrystal {

    public NegateOperatorCrystal() {
        super(TokenType.NEGATE.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal operand) {
        AtonementCrystal result = null;
        if (operand instanceof NumberCrystal<?> numberCrystal) {
            result = Arithmetic.negate(numberCrystal);
        }
        return result;
    }

}
