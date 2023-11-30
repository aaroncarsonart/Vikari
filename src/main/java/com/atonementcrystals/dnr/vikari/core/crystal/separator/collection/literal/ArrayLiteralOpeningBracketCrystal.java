package com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left square bracket [ denotes the beginning of an array literal after $:.
 */
public class ArrayLiteralOpeningBracketCrystal extends AtonementCrystal {

    public ArrayLiteralOpeningBracketCrystal() {
        super(TokenType.ARRAY_LITERAL_OPENING_BRACKET.getIdentifier());
    }

}
