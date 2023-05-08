package com.atonement.crystals.dnr.vikari.core.crystal.separator.grouping;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The use of pairs of square brackets [ ] denotes grouping.
 * Use grouping on expressions to forces evaluation different
 * from the default operator precedence rules.<br/>
 * <br/>
 * And, in some cases, they are used to group together related
 * statements. Such as for certain control-flow statements.<br/>
 * <br/>
 * They are functionally equivalent to the usual use of
 * parentheses ( ) for grouping in other programming languages.
 */
public class RightSquareBracketCrystal extends AtonementCrystal {

    public RightSquareBracketCrystal() {
        super(TokenType.RIGHT_SQUARE_BRACKET.getIdentifier());
    }

}
