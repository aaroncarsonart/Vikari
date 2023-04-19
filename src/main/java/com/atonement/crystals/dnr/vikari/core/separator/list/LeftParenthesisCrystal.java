package com.atonement.crystals.dnr.vikari.core.separator.list;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * A left-parenthesis ( denotes the beginning of a list literal.
 */
public class LeftParenthesisCrystal extends AtonementCrystal {

    public LeftParenthesisCrystal() {
        super(TokenType.LEFT_PARENTHESIS.getIdentifier());
    }

}
