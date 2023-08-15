package com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The static super operator # accepts an integer as its left
 * operand to access static members of a type's parent that many
 * levels up the inheritance hierarchy.
 */
public class StaticSuperOperatorCrystal extends AtonementCrystal {

    public StaticSuperOperatorCrystal() {
        super(TokenType.STATIC_SUPER.getIdentifier());
    }

}
