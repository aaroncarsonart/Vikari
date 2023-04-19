package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The left subtract assignment operator -<< offers a shorthand syntax for
 * subtracting the right operand from the left operand, and then assigning the
 * result to the left operand.
 */
public class LeftSubtractAssignmentOperatorCrystal extends AtonementCrystal {

    public LeftSubtractAssignmentOperatorCrystal() {
        super(TokenType.LEFT_SUBTRACT_ASSIGNMENT.getIdentifier());
    }

}