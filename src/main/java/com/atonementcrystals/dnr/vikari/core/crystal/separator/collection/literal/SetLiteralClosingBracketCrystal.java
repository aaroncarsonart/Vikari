package com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A right curly bracket } denotes the end of a set (or map) literal.
 */
public class SetLiteralClosingBracketCrystal extends AtonementCrystal {

    public SetLiteralClosingBracketCrystal() {
        super(TokenType.SET_LITERAL_CLOSING_BRACKET.getIdentifier());
    }

}
