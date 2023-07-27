package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * A NullCrystal represents a null value in Vikari. Nulls have a length,
 * which can help to differentiate between different null instances in
 * algorithms. So in effect, a NullCrystal is also an integer.
 */
public class NullCrystal extends AtonementCrystal {
    private int length;

    public NullCrystal(String identifier, int length) {
        super(identifier);
        this.length = length;
    }

    public NullCrystal(int length) {
        this(buildIdentifier(length), length);
    }

    public int getLength() {
        return length;
    }

    @Override
    public NullCrystal copy() {
        NullCrystal copy = new NullCrystal(getIdentifier(), length);
        copyFields(this, copy);
        return copy;
    }

    private static String buildIdentifier(int length) {
        return String.format("__[%d]__", length);
    }

    @Override
    public String getStringRepresentation() {
        if (length == 0) {
            return "null";
        }
        return String.format("Null::{length=%d}", length);
    }

    @Override
    public String toString() {
        return getStringRepresentation();
    }
}
