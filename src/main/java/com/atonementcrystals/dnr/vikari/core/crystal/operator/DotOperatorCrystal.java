package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The dot operator . dereferences instance field members from a crystal.
 */
public class DotOperatorCrystal extends AtonementCrystal {

    public DotOperatorCrystal() {
        super(TokenType.DOT.getIdentifier());
    }

}

