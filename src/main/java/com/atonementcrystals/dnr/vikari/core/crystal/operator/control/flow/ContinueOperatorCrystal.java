package com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The continue operator crystal >> begins a continue statement.
 */
public class ContinueOperatorCrystal extends AtonementCrystal {

    public ContinueOperatorCrystal() {
        super(TokenType.CONTINUE.getIdentifier());
    }

}
