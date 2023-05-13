package com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The break operator crystal `vv` begins a break statement.
 */
public class BreakOperatorCrystal extends AtonementCrystal {

    public BreakOperatorCrystal() {
        super(TokenType.BREAK.getIdentifier());
    }

}
