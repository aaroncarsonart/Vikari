package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The reference equals operator <=> checks if the left operand
 * refers to the same instance as the right operand.
 */
public class ReferenceEqualsOperatorCrystal extends AtonementCrystal {

    public ReferenceEqualsOperatorCrystal() {
        super(TokenType.REFERENCE_EQUALS.getIdentifier());
    }

}

