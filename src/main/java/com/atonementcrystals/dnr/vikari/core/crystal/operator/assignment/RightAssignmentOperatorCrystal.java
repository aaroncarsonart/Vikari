package com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

/**
 * The right assignment operator >> assigns the value of the left
 * operand to the right operand.
 */
public class RightAssignmentOperatorCrystal extends BinaryOperatorCrystal {

    public RightAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_ASSIGNMENT.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        throw new UnsupportedOperationException("Assignment operators are not evaluated using the evaluate() method.");
    }

}
