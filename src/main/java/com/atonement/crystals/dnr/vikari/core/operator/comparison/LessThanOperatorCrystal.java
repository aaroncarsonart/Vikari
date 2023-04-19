package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The less than operator < checks if the left operand is less
 * than the right operand.
 */
public class LessThanOperatorCrystal extends AtonementCrystal {

    public LessThanOperatorCrystal() {
        super(TokenType.LESS_THAN.getIdentifier());
    }

}
