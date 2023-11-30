package com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left curly bracket { denotes the beginning of a set (or map) literal after $:.
 */
public class SetLiteralOpeningBracketCrystal extends AtonementCrystal {

    public SetLiteralOpeningBracketCrystal() {
        super(TokenType.SET_LITERAL_OPENING_BRACKET.getIdentifier());
    }

}
