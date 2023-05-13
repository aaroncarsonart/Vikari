package com.atonementcrystals.dnr.vikari.core.crystal.operator.math;

import com.atonementcrystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The left divide operator / divides the left operand by the right operand.
 */
public class LeftDivideOperatorCrystal extends BinaryOperatorCrystal {

    public LeftDivideOperatorCrystal() {
        super(TokenType.LEFT_DIVIDE.getIdentifier());
    }

}
