package com.atonement.crystals.dnr.vikari.core.separator.list;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * A right-parenthesis ) denotes the ending of a list literal.
 */
public class RightParenthesisCrystal extends AtonementCrystal {

    public RightParenthesisCrystal() {
        super(TokenType.RIGHT_PARENTHESIS.getIdentifier());
    }

}
