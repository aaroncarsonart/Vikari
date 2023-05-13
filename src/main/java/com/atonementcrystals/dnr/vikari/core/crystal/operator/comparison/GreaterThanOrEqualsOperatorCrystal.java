package com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The greater than or equals operator >= checks if the left operand is
 * greater than or equal to the right operand.
 */
public class GreaterThanOrEqualsOperatorCrystal extends AtonementCrystal {

    public GreaterThanOrEqualsOperatorCrystal() {
        super(TokenType.GREATER_THAN_OR_EQUALS.getIdentifier());
    }

}
