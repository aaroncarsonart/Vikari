package com.atonement.crystals.dnr.vikari.core.operator.assignment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The left assignment operator `<<` assigns the value of the right-hand
 * operand into the identifier of the left-hand operand.
 */
public class LeftAssignmentOperatorCrystal extends AtonementCrystal {
    public LeftAssignmentOperatorCrystal() {
        super(DefaultIdentifierMapping.LEFT_ASSIGNMENT.getIdentifier());
    }
}
