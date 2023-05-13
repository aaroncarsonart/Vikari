package com.atonementcrystals.dnr.vikari.core.crystal.operator.logical;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The logical OR operator " applies the logical OR operation to
 * the left and right operands. This performs short-circuiting, so
 * the second operand is only evaluated if the left operand evaluates
 * to false.
 */
public class LogicalOrOperatorCrystal extends AtonementCrystal {

    public LogicalOrOperatorCrystal() {
        super(TokenType.LOGICAL_OR.getIdentifier());
    }

}
