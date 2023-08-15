package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The concatenate operator + appends two values together into a string.
 * At least one operand must itself be a string.
 */
public class ConcatenateOperatorCrystal extends AtonementCrystal {

    public ConcatenateOperatorCrystal() {
        super(TokenType.CONCATENATE.getIdentifier());
    }

}