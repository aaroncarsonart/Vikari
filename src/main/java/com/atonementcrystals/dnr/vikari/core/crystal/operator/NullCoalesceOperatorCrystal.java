package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The null coalesce operator ? returns the left operand unless it is null,
 * in which case it then instead returns the right operand.
 */
public class NullCoalesceOperatorCrystal extends BinaryOperatorCrystal {

    public NullCoalesceOperatorCrystal() {
        super(TokenType.NULL_COALESCE.getIdentifier());
    }

}
