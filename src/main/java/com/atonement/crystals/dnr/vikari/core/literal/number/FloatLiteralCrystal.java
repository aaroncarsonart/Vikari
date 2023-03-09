package com.atonement.crystals.dnr.vikari.core.literal.number;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

/**
 * Float literal identifiers are floating-point numbers that match a
 * numeric value within the 32-bit range of a float datatype in Java.
 */
public class FloatLiteralCrystal extends AtonementCrystal {

    private float value;

    public FloatLiteralCrystal(String identifier) {
        super(identifier);
    }

    public float getValue() {
        return value;
    }

    public void setValue(float value) {
        this.value = value;
    }
}
