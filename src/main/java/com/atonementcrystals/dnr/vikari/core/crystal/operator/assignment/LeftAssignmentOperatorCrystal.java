package com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The left assignment operator << assigns the value of the right
 * operand to the left operand.
 */
public class LeftAssignmentOperatorCrystal extends AtonementCrystal {

    public LeftAssignmentOperatorCrystal() {
        super(TokenType.LEFT_ASSIGNMENT.getIdentifier());
    }

}
