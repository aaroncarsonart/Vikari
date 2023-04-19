package com.atonement.crystals.dnr.vikari.core.keyword.error;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The catch crystal `++` begins a catch statement.
 */
public class CatchCrystal extends AtonementCrystal {

    public CatchCrystal() {
        super(TokenType.CATCH.getIdentifier());
    }

}
