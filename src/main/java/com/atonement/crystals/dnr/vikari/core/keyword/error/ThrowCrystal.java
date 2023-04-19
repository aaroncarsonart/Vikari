package com.atonement.crystals.dnr.vikari.core.keyword.error;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The throw crystal `--` begins a throw statement.
 */
public class ThrowCrystal extends AtonementCrystal {

    public ThrowCrystal() {
        super(TokenType.THROW.getIdentifier());
    }

}
