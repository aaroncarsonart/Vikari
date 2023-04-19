package com.atonement.crystals.dnr.vikari.core.operator.control.flow;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The continue operator crystal >> begins a continue statement.
 */
public class ContinueOperatorCrystal extends AtonementCrystal {

    public ContinueOperatorCrystal() {
        super(TokenType.CONTINUE.getIdentifier());
    }

}
