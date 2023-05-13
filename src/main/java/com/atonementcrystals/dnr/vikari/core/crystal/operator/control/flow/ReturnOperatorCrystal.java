package com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The return operator crystal ^^ begins a return statement.
 */
public class ReturnOperatorCrystal extends AtonementCrystal {

    public ReturnOperatorCrystal() {
        super(TokenType.RETURN.getIdentifier());
    }

}