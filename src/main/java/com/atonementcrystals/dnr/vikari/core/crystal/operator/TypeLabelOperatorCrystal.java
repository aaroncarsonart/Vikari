package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The type label operator : denotes static typing in a variable declaration statement.
 */
public class TypeLabelOperatorCrystal extends AtonementCrystal {

    public TypeLabelOperatorCrystal() {
        super(TokenType.TYPE_LABEL.getIdentifier());
    }

}

