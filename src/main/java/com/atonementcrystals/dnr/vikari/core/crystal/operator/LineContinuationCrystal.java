package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The line continuation operator ~ allows for individual statements to be
 * defined across multiple lines.
 */
public class LineContinuationCrystal extends AtonementCrystal {

    public LineContinuationCrystal() {
        super(TokenType.LINE_CONTINUATION.getIdentifier());
    }

}