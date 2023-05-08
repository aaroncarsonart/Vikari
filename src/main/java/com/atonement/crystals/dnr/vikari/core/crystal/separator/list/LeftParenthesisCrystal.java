package com.atonement.crystals.dnr.vikari.core.crystal.separator.list;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left-parenthesis ( denotes the beginning of a list literal.
 */
public class LeftParenthesisCrystal extends AtonementCrystal {

    public LeftParenthesisCrystal() {
        super(TokenType.LEFT_PARENTHESIS.getIdentifier());
    }

}
