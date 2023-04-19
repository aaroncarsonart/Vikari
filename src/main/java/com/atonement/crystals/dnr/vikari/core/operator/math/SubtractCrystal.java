package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The subtract operator - subtracts the right operand
 * from the left operand.
 */
public class SubtractCrystal extends AtonementCrystal {

    public SubtractCrystal() {
        super(TokenType.SUBTRACT.getIdentifier());
    }

}
