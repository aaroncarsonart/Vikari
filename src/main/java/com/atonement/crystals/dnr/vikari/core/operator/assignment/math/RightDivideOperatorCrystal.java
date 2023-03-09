package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The right divide assignment operator `\>>` offers a shorthand syntax for
 * dividing the right-hand operand by the left, and then assigning the result to the
 * left-hand operand.
 */
public class RightDivideOperatorCrystal extends AtonementCrystal {
    public RightDivideOperatorCrystal() {
        super(DefaultIdentifierMapping.RIGHT_DIVIDE_ASSIGNMENT.getIdentifier());
    }
}
