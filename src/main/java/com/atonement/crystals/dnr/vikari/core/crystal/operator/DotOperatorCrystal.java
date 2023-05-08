package com.atonement.crystals.dnr.vikari.core.crystal.operator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The dot operator . de-references type members from a crystal's field.
 */
public class DotOperatorCrystal extends AtonementCrystal {

    public DotOperatorCrystal() {
        super(TokenType.DOT.getIdentifier());
    }

}

