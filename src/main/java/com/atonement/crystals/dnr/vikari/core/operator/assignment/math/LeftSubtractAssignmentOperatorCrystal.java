package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left subtract assignment operator `-<<` offers a shorthand syntax for
 * subtracting the left operand from the right operand, and then assigning the
 * result to the left-hand operand.
 */
public class LeftSubtractAssignmentOperatorCrystal extends AtonementCrystal {
    public LeftSubtractAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.LEFT_SUBTRACT_ASSIGNMENT.getIdentifier());
    }
}