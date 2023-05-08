package com.atonement.crystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The static super operator # accepts an integer as its right
 * operand to access static members of a type's parent that many
 * levels up the inheritance hierarchy.
 */
public class StaticSuperOperatorCrystal extends AtonementCrystal {

    public StaticSuperOperatorCrystal() {
        super(TokenType.STATIC_SUPER.getIdentifier());
    }

}
