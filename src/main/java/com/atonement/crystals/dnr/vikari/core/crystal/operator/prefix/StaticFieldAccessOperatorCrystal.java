package com.atonement.crystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

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
