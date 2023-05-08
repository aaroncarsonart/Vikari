package com.atonement.crystals.dnr.vikari.core.crystal.operator.control.flow;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The return operator crystal ^^ begins a return statement.
 */
public class ReturnOperatorCrystal extends AtonementCrystal {

    public ReturnOperatorCrystal() {
        super(TokenType.RETURN.getIdentifier());
    }

}