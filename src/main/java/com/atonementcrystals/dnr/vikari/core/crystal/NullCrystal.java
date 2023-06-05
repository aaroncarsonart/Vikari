package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A NullCrystal represents a null value in Vikari. Nulls have a length,
 * which can help to differentiate between different null instances in
 * algorithms. So in effect, a NullCrystal is also an unsigned integer.
 */
public class NullCrystal extends AtonementCrystal {
    private int length;

    public NullCrystal(int length) {
        super(buildSwordIdentifier(length));
        this.length = length;
    }

    public int getLength() {
        return length;
    }

    @Override
    public NullCrystal copy() {
        NullCrystal copy = new NullCrystal(length);
        copyFields(this, copy);
        return copy;
    }

    private static String buildSwordIdentifier(int length) {
        return TokenType.SWORD.getIdentifier().repeat(length);
    }

    @Override
    public String getStringRepresentation() {
        return String.format("{%d}", length);
    }

    @Override
    public String toString() {
        return getStringRepresentation();
    }
}
