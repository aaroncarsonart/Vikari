package com.atonement.crystals.dnr.vikari.core.operator.assignment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The right assignment operator `>>` assigns the value of the left-hand
 * operand into the identifier of the right-hand operand.
 */
public class RightAssignmentOperatorCrystal extends AtonementCrystal {
    public RightAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.RIGHT_ASSIGNMENT.getIdentifier());
    }
}
