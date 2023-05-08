package com.atonement.crystals.dnr.vikari.core.crystal.operator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The key value pair operator => instantiates a new Pair instance
 * where the left operand is the key, and the right operand is the value.
 */
public class KeyValuePairOperatorCrystal extends AtonementCrystal {

    public KeyValuePairOperatorCrystal() {
        super(TokenType.KEY_VALUE_PAIR.getIdentifier());
    }

}
