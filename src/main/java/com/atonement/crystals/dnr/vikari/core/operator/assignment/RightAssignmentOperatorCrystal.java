package com.atonement.crystals.dnr.vikari.core.operator.assignment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The right assignment operator >> assigns the value of the left
 * operand to the right operand.
 */
public class RightAssignmentOperatorCrystal extends AtonementCrystal {

    public RightAssignmentOperatorCrystal() {
        super(TokenType.RIGHT_ASSIGNMENT.getIdentifier());
    }

}
