package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The right add assignment operator +>> offers a shorthand syntax for adding
 * the left and right operands together, and then assigning the result to the
 * right operand.
 */
public class RightAddAssignmentOperatorCrystal extends AtonementCrystal {

    public RightAddAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_ADD_ASSIGNMENT.getIdentifier());
    }

}
