package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left divide operator `/` divides the right operand by the left.
 */
public class LeftDivideOperatorCrystal extends AtonementCrystal {
    public LeftDivideOperatorCrystal() {
        super(DefaultIdentifierMapping.LEFT_DIVIDE.getIdentifier());
    }
}