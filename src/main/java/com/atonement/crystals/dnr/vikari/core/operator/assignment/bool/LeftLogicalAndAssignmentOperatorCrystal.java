package com.atonement.crystals.dnr.vikari.core.operator.assignment.bool;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left logical AND assignment operator `^<<` offers a shorthand syntax for
 * performing a logical AND operation on the left and right operands, and then
 * assigning the result to the left-hand operand.
 */
public class LeftLogicalAndAssignmentOperatorCrystal extends AtonementCrystal {
    public LeftLogicalAndAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.LEFT_LOGICAL_AND_ASSIGNMENT.getIdentifier());
    }
}
