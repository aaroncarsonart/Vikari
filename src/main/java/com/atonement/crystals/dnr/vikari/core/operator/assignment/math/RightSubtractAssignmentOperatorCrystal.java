package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left subtract assignment operator `->>` offers a shorthand syntax for
 * subtracting the left-hand operand from the right, and then assigning the
 * result to the right-hand operand.
 */
public class RightSubtractAssignmentOperatorCrystal extends AtonementCrystal {
    public RightSubtractAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.RIGHT_SUBTRACT_ASSIGNMENT.getIdentifier());
    }
}
