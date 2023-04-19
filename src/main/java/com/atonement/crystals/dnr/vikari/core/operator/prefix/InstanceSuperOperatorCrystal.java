package com.atonement.crystals.dnr.vikari.core.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

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
