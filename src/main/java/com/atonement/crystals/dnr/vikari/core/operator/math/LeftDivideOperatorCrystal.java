package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The left divide operator / divides the left operand by the right operand.
 */
public class LeftDivideOperatorCrystal extends AtonementCrystal {

    public LeftDivideOperatorCrystal() {
        super(TokenType.LEFT_DIVIDE.getIdentifier());
    }

}
