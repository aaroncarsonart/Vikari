package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The right subtract assignment operator ->> offers a shorthand syntax for
 * subtracting the left operand from the right operand, and then assigning the
 * result to the right operand.
 */
public class RightSubtractAssignmentOperatorCrystal extends AtonementCrystal {

    public RightSubtractAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_SUBTRACT_ASSIGNMENT.getIdentifier());
    }

}
