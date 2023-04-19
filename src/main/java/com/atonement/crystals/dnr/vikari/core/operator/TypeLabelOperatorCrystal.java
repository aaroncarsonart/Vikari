package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The type label operator : denotes static typing in a variable declaration statement.
 */
public class TypeLabelOperatorCrystal extends AtonementCrystal {

    public TypeLabelOperatorCrystal() {
        super(TokenType.TYPE_LABEL.getIdentifier());
    }

}

