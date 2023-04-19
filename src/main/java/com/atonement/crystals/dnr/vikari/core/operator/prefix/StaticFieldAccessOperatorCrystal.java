package com.atonement.crystals.dnr.vikari.core.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The unary static field access operator # is used in two cases:
 * <ol>
 *     <li>Declare a static type member.</li>
 *     <li>Access a static type member.</li>
 * </ol>
 * # Refers to the DNR type outside of a type declaraton.
 */
public class StaticFieldAccessOperatorCrystal extends AtonementCrystal {

    public StaticFieldAccessOperatorCrystal() {
        super(TokenType.STATIC_FIELD_ACCESS.getIdentifier());
    }

}
