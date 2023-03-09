package com.atonement.crystals.dnr.vikari.core.operator.assignment.bool;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The right logical OR assignment operator `">>` offers a shorthand syntax for
 * performing a logical OR operation on the left and right operands, and then
 * assigning the result to the left-hand operand.
 */
public class RightOrAssignmentOperatorCrystal extends AtonementCrystal {
    public RightOrAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.RIGHT_LOGICAL_OR_ASSIGNMENT.getIdentifier());
    }
}
