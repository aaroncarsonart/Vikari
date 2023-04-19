package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The ADD operator + adds the left and right operands.
 */
public class AddOperatorCrystal extends AtonementCrystal {

    public AddOperatorCrystal() {
        super(TokenType.ADD.getIdentifier());
    }

}
