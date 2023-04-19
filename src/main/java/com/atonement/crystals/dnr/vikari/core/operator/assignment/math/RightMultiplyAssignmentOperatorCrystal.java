package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The right multiply assignment operator *>> offers a shorthand syntax for
 * multiplying the left operand by the right operand, and then assigning
 * the result to the right operand.
 */
public class RightMultiplyAssignmentOperatorCrystal extends AtonementCrystal {

    public RightMultiplyAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_MULTIPLY_ASSIGNMENT.getIdentifier());
    }

}
