package com.atonement.crystals.dnr.vikari.core.crystal.operator.math;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The percent operator % defines a special Percentage type which is a
 * Decimal type, but it is always represented as: (value * 100). So a
 * Percentage with value 50% when multiplied by any number would be
 * evaluated as multiplying that number by 0.5.
 */
public class PercentCrystal extends AtonementCrystal {

    public PercentCrystal() {
            super(TokenType.PERCENT.getIdentifier());
        }

}
