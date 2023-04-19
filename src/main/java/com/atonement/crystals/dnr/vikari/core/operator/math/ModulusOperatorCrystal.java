package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/*
 * The modulus operator % returns the remainder of the left operand
 * divided by the right operand.
 */
public class ModulusOperatorCrystal extends AtonementCrystal {

    public ModulusOperatorCrystal() {
        super(TokenType.MODULUS.getIdentifier());
    }

}
