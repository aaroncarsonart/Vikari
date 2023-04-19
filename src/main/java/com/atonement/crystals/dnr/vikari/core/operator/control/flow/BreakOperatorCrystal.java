package com.atonement.crystals.dnr.vikari.core.operator.control.flow;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The break operator crystal `vv` begins a break statement.
 */
public class BreakOperatorCrystal extends AtonementCrystal {

    public BreakOperatorCrystal() {
        super(TokenType.BREAK.getIdentifier());
    }

}
