package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The left divide assignment operator /<< offers a shorthand syntax for dividing
 * the left operand by the right operand, and then assigning the result to the
 * left operand.
 */
public class LeftDivideAssignmentOperatorCrystal extends AtonementCrystal {

    public LeftDivideAssignmentOperatorCrystal() {
        super(TokenType.LEFT_DIVIDE_ASSIGNMENT.getIdentifier());
    }

}
