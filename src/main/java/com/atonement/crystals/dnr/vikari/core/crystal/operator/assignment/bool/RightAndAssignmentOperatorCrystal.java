package com.atonement.crystals.dnr.vikari.core.crystal.operator.assignment.bool;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The right logical AND assignment operator ^>> offers a shorthand syntax for
 * performing a logical AND operation on the left and right operands, and then
 * assigning the result to the right operand.
 */
public class RightAndAssignmentOperatorCrystal extends AtonementCrystal {

    public RightAndAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_LOGICAL_AND_ASSIGNMENT.getIdentifier());
    }

}
