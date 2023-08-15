package com.atonementcrystals.dnr.vikari.core.crystal.separator.grouping;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The use of pairs of square brackets [ ] denotes grouping.
 * Use grouping on expressions to force evaluation different
 * from the default operator precedence rules.<br/>
 * <br/>
 * And, in some cases, they are used to group together related
 * statements. Such as for certain control-flow statements.<br/>
 * <br/>
 * They are functionally equivalent to the usual use of
 * parentheses ( ) for grouping in other programming languages.
 */
public class LeftSquareBracketCrystal extends AtonementCrystal {

    public LeftSquareBracketCrystal() {
        super(TokenType.LEFT_SQUARE_BRACKET.getIdentifier());
    }

}
