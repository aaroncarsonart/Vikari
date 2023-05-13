package com.atonementcrystals.dnr.vikari.core.crystal.literal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * Double literal identifiers are floating-point numbers that match a
 * numeric value within the 64-bit range of a double datatype in Java.
 */
public class DoubleLiteralCrystal extends AtonementCrystal {

    private Double value;

    public DoubleLiteralCrystal(String identifier) {
        super(identifier);
    }

    public Double getValue() {
        return value;
    }

    public void setValue(Double value) {
        this.value = value;
    }
}
