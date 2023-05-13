package com.atonementcrystals.dnr.vikari.core.crystal.literal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * Integer literal identifiers are integer numbers that match a
 * numeric value within the 32-bit range of an int datatype in java.
 */
public class IntegerLiteralCrystal extends AtonementCrystal {

    private Integer value;

    public IntegerLiteralCrystal(String identifier) {
        super(identifier);
    }

    public int getValue() {
        return value;
    }

    public void setValue(Integer value) {
        this.value = value;
    }
}
