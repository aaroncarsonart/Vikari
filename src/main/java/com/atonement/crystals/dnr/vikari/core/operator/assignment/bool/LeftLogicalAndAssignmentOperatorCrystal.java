package com.atonement.crystals.dnr.vikari.core.operator.assignment.bool;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The left logical AND assignment operator ^<< offers a shorthand syntax for
 * performing a logical AND operation on the left and right operands, and then
 * assigning the result to the left operand.
 */
public class LeftLogicalAndAssignmentOperatorCrystal extends AtonementCrystal {

    public LeftLogicalAndAssignmentOperatorCrystal() {
        super(TokenType.LEFT_LOGICAL_AND_ASSIGNMENT.getIdentifier());
    }

}
