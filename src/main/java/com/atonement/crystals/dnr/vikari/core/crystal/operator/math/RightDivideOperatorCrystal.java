package com.atonement.crystals.dnr.vikari.core.crystal.operator.math;

import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The right divide operator \ divides the right operand by the left operand.
 */
public class RightDivideOperatorCrystal extends BinaryOperatorCrystal {

    public RightDivideOperatorCrystal() {
        super(TokenType.RIGHT_DIVIDE.getIdentifier());
    }

}
