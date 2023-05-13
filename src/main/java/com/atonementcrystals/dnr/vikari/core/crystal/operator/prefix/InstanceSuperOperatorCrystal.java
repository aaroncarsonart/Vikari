package com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The instance super operator @ accepts an integer as its right
 * operand to access instance members of a type's parent that many
 * levels up the inheritance hierarchy.
 */
public class InstanceSuperOperatorCrystal extends AtonementCrystal {

    public InstanceSuperOperatorCrystal() {
        super(TokenType.INSTANCE_SUPER.getIdentifier());
    }

}
