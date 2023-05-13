package com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

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
