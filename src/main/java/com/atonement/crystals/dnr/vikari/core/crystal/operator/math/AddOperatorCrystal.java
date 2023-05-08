package com.atonement.crystals.dnr.vikari.core.crystal.operator.math;

import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The ADD operator + adds the left and right operands.
 */
public class AddOperatorCrystal extends BinaryOperatorCrystal {

    public AddOperatorCrystal() {
        super(TokenType.ADD.getIdentifier());
    }

}
