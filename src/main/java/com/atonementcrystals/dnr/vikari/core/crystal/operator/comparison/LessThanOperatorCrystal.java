package com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The less than operator < checks if the left operand is less
 * than the right operand.
 */
public class LessThanOperatorCrystal extends AtonementCrystal {

    public LessThanOperatorCrystal() {
        super(TokenType.LESS_THAN.getIdentifier());
    }

}
