package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left divide assignment operator `/<<` offers a shorthand syntax for dividing
 * the left operand by the right operand, and then assigning the result to the
 * left-hand operand.
 */
public class LeftDivideAssignmentOperatorCrystal extends AtonementCrystal {
    public LeftDivideAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.LEFT_DIVIDE_ASSIGNMENT.getIdentifier());
    }
}