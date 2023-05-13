package com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The instance of operator -> checks if the left operand is an
 * instance of the right operand. The right operand must be a type.
 */
public class InstanceOfOperatorCrystal extends AtonementCrystal {

    public InstanceOfOperatorCrystal() {
        super(TokenType.INSTANCE_OF.getIdentifier());
    }

}
