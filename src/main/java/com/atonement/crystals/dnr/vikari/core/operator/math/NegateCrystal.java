package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The unary negate operator - negates the right operand.
 */
public class NegateCrystal extends AtonementCrystal {

    public NegateCrystal() {
        super(TokenType.NEGATE.getIdentifier());
    }

}
