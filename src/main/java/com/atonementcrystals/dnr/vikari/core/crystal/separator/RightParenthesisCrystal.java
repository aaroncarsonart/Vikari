package com.atonementcrystals.dnr.vikari.core.crystal.separator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A right-parenthesis ) denotes the ending of a list literal.
 */
public class RightParenthesisCrystal extends AtonementCrystal {

    public RightParenthesisCrystal() {
        super(TokenType.RIGHT_PARENTHESIS.getIdentifier());
    }

}
