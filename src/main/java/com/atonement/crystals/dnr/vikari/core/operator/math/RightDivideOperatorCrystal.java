package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The right divide operator `\` divides the left operand by the right.
 */
public class RightDivideOperatorCrystal extends AtonementCrystal {
    public RightDivideOperatorCrystal() {
        super(DefaultIdentifierMapping.RIGHT_DIVIDE.getIdentifier());
    }
}
