package com.atonement.crystals.dnr.vikari.core.crystal.operator.assignment.math;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

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