package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The greater than or equals operator >= checks if the left operand is
 * greater than or equal to the right operand.
 */
public class GreaterThanOrEqualsOperatorCrystal extends AtonementCrystal {

    public GreaterThanOrEqualsOperatorCrystal() {
        super(TokenType.GREATER_THAN_OR_EQUALS.getIdentifier());
    }

}
