package com.atonement.crystals.dnr.vikari.core.operator.control.flow;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The return operator crystal ^^ begins a return statement.
 */
public class ReturnOperatorCrystal extends AtonementCrystal {

    public ReturnOperatorCrystal() {
        super(TokenType.RETURN.getIdentifier());
    }

}