package com.atonement.crystals.dnr.vikari.core.operator.angelguard;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The catch-all crystal || begins a catch-all statement. Which
 * automatically handles all exceptions after a try statement.
 * It can also be used for a default case when a throw statement
 * is used for a switch-case pattern matching sequence of statements.
 */
public class CatchAllCrystal extends AtonementCrystal {

    public CatchAllCrystal() {
        super(TokenType.CATCH_ALL.getIdentifier());
    }

}
