package com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A right-parenthesis ) denotes the end of a list literal.
 */
public class ListLiteralClosingBracketCrystal extends AtonementCrystal {

    public ListLiteralClosingBracketCrystal() {
        super(TokenType.LIST_LITERAL_CLOSING_BRACKET.getIdentifier());
    }

}
