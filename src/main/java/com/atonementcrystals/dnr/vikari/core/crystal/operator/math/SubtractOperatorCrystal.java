package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The subtract operator - subtracts the right operand
 * from the left operand.
 */
public class SubtractOperatorCrystal extends BinaryOperatorCrystal {

    public SubtractOperatorCrystal() {
        super(TokenType.SUBTRACT.getIdentifier());
    }

}
