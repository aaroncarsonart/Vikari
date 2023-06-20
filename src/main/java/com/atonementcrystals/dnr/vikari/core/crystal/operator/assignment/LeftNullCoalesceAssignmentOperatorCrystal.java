package com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

/**
 * The left null coalesce assignment operator ?<< assigns the value of the right
 * operand to the left operand, but only if the left operand is null.
 */
public class LeftNullCoalesceAssignmentOperatorCrystal extends BinaryOperatorCrystal {

    public LeftNullCoalesceAssignmentOperatorCrystal() {
        super(TokenType.LEFT_NULL_COALESCE_ASSIGNMENT.getIdentifier());
    }

}
