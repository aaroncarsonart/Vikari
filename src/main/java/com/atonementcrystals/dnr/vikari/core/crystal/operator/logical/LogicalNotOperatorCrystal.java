package com.atonementcrystals.dnr.vikari.core.crystal.operator.logical;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The unary logical NOT operator ' applies the logical NOT operation
 * to the right operand.
 */
public class LogicalNotOperatorCrystal extends AtonementCrystal {

    public LogicalNotOperatorCrystal() {
        super(TokenType.LOGICAL_NOT.getIdentifier());
    }

}
