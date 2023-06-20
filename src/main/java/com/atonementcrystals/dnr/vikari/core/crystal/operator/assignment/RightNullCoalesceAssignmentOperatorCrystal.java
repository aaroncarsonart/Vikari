package com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

/**
 * The right null coalesce assignment operator ?>> assigns the value of the left
 * operand to the right operand, but only if the right operand is null.
 */
public class RightNullCoalesceAssignmentOperatorCrystal extends BinaryOperatorCrystal {

    public RightNullCoalesceAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_NULL_COALESCE_ASSIGNMENT.getIdentifier());
    }

}
