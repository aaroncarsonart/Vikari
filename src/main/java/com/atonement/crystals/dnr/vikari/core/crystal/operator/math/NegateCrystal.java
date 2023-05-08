package com.atonement.crystals.dnr.vikari.core.crystal.operator.math;

import com.atonement.crystals.dnr.vikari.core.crystal.UnaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The unary negate operator - negates the right operand.
 */
public class NegateCrystal extends UnaryOperatorCrystal {

    public NegateCrystal() {
        super(TokenType.NEGATE.getIdentifier());
    }

}
