package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The equals operator = checks if the left operand is equal to
 * the right operand by calling <code>left.equals!(right)</code>
 * as a null-safe operation.
 */
public class EqualsOperatorCrystal extends AtonementCrystal {

    public EqualsOperatorCrystal() {
        super(TokenType.EQUALS.getIdentifier());
    }

}

