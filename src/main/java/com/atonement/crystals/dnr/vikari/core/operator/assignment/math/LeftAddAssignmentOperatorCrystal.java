package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The left add assignment operator +<< offers a shorthand syntax for adding
 * the left and right operands together, and then assigning the result to the
 * left operand.
 */
public class LeftAddAssignmentOperatorCrystal extends AtonementCrystal {

    public LeftAddAssignmentOperatorCrystal() {
        super(TokenType.LEFT_ADD_ASSIGNMENT.getIdentifier());
    }

}