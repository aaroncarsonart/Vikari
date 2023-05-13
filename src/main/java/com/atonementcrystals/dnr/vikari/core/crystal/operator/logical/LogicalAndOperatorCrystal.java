package com.atonementcrystals.dnr.vikari.core.crystal.operator.logical;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The logical AND operator ^ applies the logical AND operation to
 * the left and right operands. This performs short-circuiting, so
 * the second operand is only evaluated if the left operand evaluates
 * to true.
 */
public class LogicalAndOperatorCrystal extends AtonementCrystal {

    public LogicalAndOperatorCrystal() {
        super(TokenType.LOGICAL_AND.getIdentifier());
    }

}
