package com.atonementcrystals.dnr.vikari.core.crystal.operator.logical;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.UnaryOperatorCrystal;

/**
 * The unary logical NOT operator ' applies the logical NOT operation
 * to the right operand.
 */
public class LogicalNotOperatorCrystal extends UnaryOperatorCrystal {

    public LogicalNotOperatorCrystal() {
        super(TokenType.LOGICAL_NOT.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal operand) {
        AtonementCrystal result = null;
        if (operand instanceof BooleanCrystal booleanCrystal) {
            boolean negatedValue = !booleanCrystal.getValue();
            result = new BooleanCrystal(negatedValue);
            result.setCoordinates(operand.getCoordinates());
        }
        return result;
    }

}
