package com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

/**
 * The left assignment operator << assigns the value of the right
 * operand to the left operand.
 */
public class LeftAssignmentOperatorCrystal extends BinaryOperatorCrystal {

    public LeftAssignmentOperatorCrystal() {
        super(TokenType.LEFT_ASSIGNMENT.getIdentifier());
    }

}
