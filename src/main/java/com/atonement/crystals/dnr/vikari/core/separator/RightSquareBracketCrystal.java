package com.atonement.crystals.dnr.vikari.core.separator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The use of pairs of square brackets [,] denotes which parts of
 * Vikari code statements are to be evaluated before all others.
 * They are functionally equivalent to the usual use of parentheses
 * (,) in other programming languages.
 */
public class RightSquareBracketCrystal extends AtonementCrystal {
    public RightSquareBracketCrystal() {
        super(DefaultIdentifierMapping.RIGHT_SQUARE_BRACKET.getIdentifier());
    }
}
