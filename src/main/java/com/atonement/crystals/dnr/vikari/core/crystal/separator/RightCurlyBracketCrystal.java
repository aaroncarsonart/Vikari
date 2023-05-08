package com.atonement.crystals.dnr.vikari.core.crystal.separator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A variable name in a declaration enclosed in a pair of curly
 * brackets { } denotes that variable is a constant.<br/>
 * <br/>
 * Constants cannot be reassigned. Functions overloaded with different
 * type signatures can be defined as constant on an individual
 * basis. Constant functions cannot be overridden in subclasses.<br/>
 * <br/>
 * Curly brackets are also used in declaring use of an
 * <code>${Annotations}</code> and flexible expressions.
 */
public class RightCurlyBracketCrystal extends AtonementCrystal {

    public RightCurlyBracketCrystal() {
        super(TokenType.RIGHT_CURLY_BRACKET.getIdentifier());
    }

}
