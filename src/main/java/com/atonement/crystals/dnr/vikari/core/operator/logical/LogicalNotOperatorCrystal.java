package com.atonement.crystals.dnr.vikari.core.operator.logical;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The unary logical NOT operator ' applies the logical NOT operation
 * to the right operand.
 */
public class LogicalNotOperatorCrystal extends AtonementCrystal {

    public LogicalNotOperatorCrystal() {
        super(TokenType.LOGICAL_NOT.getIdentifier());
    }

}
