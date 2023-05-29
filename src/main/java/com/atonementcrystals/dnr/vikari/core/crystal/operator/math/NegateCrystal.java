package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.operator.UnaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The unary negate operator - negates the right operand.
 */
public class NegateCrystal extends UnaryOperatorCrystal {

    public NegateCrystal() {
        super(TokenType.NEGATE.getIdentifier());
    }

}
