package com.atonementcrystals.dnr.vikari.core.crystal.keyword.error;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The catch crystal `++` begins a catch statement.
 */
public class CatchCrystal extends AtonementCrystal {

    public CatchCrystal() {
        super(TokenType.CATCH.getIdentifier());
    }

}
