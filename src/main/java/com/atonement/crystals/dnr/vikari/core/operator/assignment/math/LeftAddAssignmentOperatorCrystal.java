package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left add assignment operator `+<<` offers a shorthand syntax for adding
 * the left and right operands together, and then assigning the result to the
 * left-hand operand.
 */
public class LeftAddAssignmentOperatorCrystal extends AtonementCrystal {
    public LeftAddAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.LEFT_ADD_ASSIGNMENT.getIdentifier());
    }
}