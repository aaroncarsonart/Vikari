package com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The reference equals operator <=> checks if the left operand
 * refers to the same instance as the right operand. Which means
 * both crystals are defined by the same AtonementField.
 */
public class ReferenceEqualsOperatorCrystal extends AtonementCrystal {

    public ReferenceEqualsOperatorCrystal() {
        super(TokenType.REFERENCE_EQUALS.getIdentifier());
    }

}

