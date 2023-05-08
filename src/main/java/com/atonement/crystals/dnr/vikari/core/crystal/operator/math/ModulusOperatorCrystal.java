package com.atonement.crystals.dnr.vikari.core.crystal.operator.math;

import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/*
 * The modulus operator % returns the remainder of the left operand
 * divided by the right operand.
 */
public class ModulusOperatorCrystal extends BinaryOperatorCrystal {

    public ModulusOperatorCrystal() {
        super(TokenType.MODULUS.getIdentifier());
    }

}
