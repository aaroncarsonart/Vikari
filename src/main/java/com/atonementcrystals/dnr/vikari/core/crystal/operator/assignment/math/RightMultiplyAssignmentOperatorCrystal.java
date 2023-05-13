package com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.math;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

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
