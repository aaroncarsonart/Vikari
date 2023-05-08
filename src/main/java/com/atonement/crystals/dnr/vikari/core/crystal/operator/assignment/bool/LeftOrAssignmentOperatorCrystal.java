package com.atonement.crystals.dnr.vikari.core.crystal.operator.assignment.bool;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The left logical OR assignment operator "<< offers a shorthand syntax for
 * performing a logical OR operation on the left and right operands, and then
 * assigning the result to the left operand.
 */
public class LeftOrAssignmentOperatorCrystal extends AtonementCrystal {

    public LeftOrAssignmentOperatorCrystal() {
        super(TokenType.LEFT_LOGICAL_OR_ASSIGNMENT.getIdentifier());
    }

}
