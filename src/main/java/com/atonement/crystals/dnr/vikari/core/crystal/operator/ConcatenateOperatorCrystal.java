package com.atonement.crystals.dnr.vikari.core.crystal.operator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The concatenate operator `+` appends two values together into a string.
 * At least one operand must itself be a string.
 */
public class ConcatenateOperatorCrystal extends AtonementCrystal {

    public ConcatenateOperatorCrystal() {
        super(TokenType.CONCATENATE.getIdentifier());
    }

}