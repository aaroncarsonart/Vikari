package com.atonement.crystals.dnr.vikari.core.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The left multiply assignment operator *<< offers a shorthand syntax for
 * multiplying the left operand by the right operand, and then assigning
 * the result to the left operand.
 */
public class LeftMultiplyAssignmentOperatorCrystal extends AtonementCrystal {

    public LeftMultiplyAssignmentOperatorCrystal() {
        super(TokenType.LEFT_MULTIPLY_ASSIGNMENT.getIdentifier());
    }

}