package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The exists ? operator crystal tests that a crystal contains a given field member.
 * given the form of ``crystal?fieldMember:Type``. The type label assertion is optional.
 */
public class ExistsOperatorCrystal extends AtonementCrystal {

    public ExistsOperatorCrystal() {
        super(TokenType.EXISTS.getIdentifier());
    }

}
