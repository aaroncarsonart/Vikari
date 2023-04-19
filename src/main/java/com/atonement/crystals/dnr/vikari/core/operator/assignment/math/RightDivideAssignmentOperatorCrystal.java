package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The right divide assignment operator \>> offers a shorthand syntax for dividing
 * the right operand by the left operand, and then assigning the result to the
 * right operand.
 */
public class RightDivideAssignmentOperatorCrystal extends AtonementCrystal {

    public RightDivideAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_DIVIDE_ASSIGNMENT.getIdentifier());
    }

}
