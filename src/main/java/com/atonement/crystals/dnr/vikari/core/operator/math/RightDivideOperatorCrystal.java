package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The right divide operator \ divides the right operand by the left operand.
 */
public class RightDivideOperatorCrystal extends AtonementCrystal {

    public RightDivideOperatorCrystal() {
        super(TokenType.RIGHT_DIVIDE.getIdentifier());
    }

}
