package com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left-parenthesis ( denotes the beginning of a list literal after $:.
 */
public class ListLiteralOpeningBracketCrystal extends AtonementCrystal {

    public ListLiteralOpeningBracketCrystal() {
        super(TokenType.LIST_LITERAL_OPENING_BRACKET.getIdentifier());
    }

}
