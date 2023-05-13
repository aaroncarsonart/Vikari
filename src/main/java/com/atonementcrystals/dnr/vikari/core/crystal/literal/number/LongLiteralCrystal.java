package com.atonementcrystals.dnr.vikari.core.crystal.literal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * Long literal identifiers are integer numbers that match a numeric
 * value within the 64-bit range of a long datatype in java.
 */
public class LongLiteralCrystal extends AtonementCrystal {

    private Long value;

    public LongLiteralCrystal(String identifier) {
        super(identifier);
    }

    public Long getValue() {
        return value;
    }

    public void setValue(Long value) {
        this.value = value;
    }
}
