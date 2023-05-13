package com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The less than or equals operator <= checks if the left operand is
 * less than or equal to the right operand.
 */
public class LessThanOrEqualsOperatorCrystal extends AtonementCrystal {

    public LessThanOrEqualsOperatorCrystal() {
        super(TokenType.LESS_THAN_OR_EQUALS.getIdentifier());
    }

}
