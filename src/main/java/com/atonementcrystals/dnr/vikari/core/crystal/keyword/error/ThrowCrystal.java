package com.atonementcrystals.dnr.vikari.core.crystal.keyword.error;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The throw crystal -- begins a throw statement.
 */
public class ThrowCrystal extends AtonementCrystal {

    public ThrowCrystal() {
        super(TokenType.THROW.getIdentifier());
    }

}
