package com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A right square bracket ] denotes the ending of an array literal.
 */
public class ArrayLiteralClosingBracketCrystal extends AtonementCrystal {

    public ArrayLiteralClosingBracketCrystal() {
        super(TokenType.ARRAY_LITERAL_CLOSING_BRACKET.getIdentifier());
    }

}
