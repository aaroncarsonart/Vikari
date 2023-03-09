package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left divide assignment operator `/<<` offers a shorthand syntax for dividing
 * the left operand by the right operand, and then assigning the result to the
 * left-hand operand.
 */
public class LeftMultiplyAssignmentOperatorCrystal extends AtonementCrystal {
    public LeftMultiplyAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.LEFT_MULTIPLY_ASSIGNMENT.getIdentifier());
    }
}