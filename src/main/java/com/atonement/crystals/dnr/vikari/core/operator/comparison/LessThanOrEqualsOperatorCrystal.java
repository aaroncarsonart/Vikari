package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The less than or equals operator <= checks if the left operand is
 * less than or equal to the right operand.
 */
public class LessThanOrEqualsOperatorCrystal extends AtonementCrystal {

    public LessThanOrEqualsOperatorCrystal() {
        super(TokenType.LESS_THAN_OR_EQUALS.getIdentifier());
    }

}
