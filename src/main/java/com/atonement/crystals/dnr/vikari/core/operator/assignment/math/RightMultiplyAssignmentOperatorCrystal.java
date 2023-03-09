package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The right add assignment operator `+>>` offers a shorthand syntax for adding
 * the left and right operands together, and then assigning the result to the
 * right-hand operand.
 */
public class RightMultiplyAssignmentOperatorCrystal extends AtonementCrystal {
    public RightMultiplyAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.RIGHT_MULTIPLY_ASSIGNMENT.getIdentifier());
    }
}
