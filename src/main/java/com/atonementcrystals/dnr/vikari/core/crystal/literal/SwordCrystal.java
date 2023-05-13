package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * Swords are identifiers consisting of only underscores.
 */
public class SwordCrystal extends AtonementCrystal {

    private int length;

    public SwordCrystal(String identifier) {
        super(identifier);
        this.length = calculateSwordLength(identifier);
    }

    private int calculateSwordLength(String identifier) {
        return identifier.length();
    }

    public int getLength() {
        return length;
    }

    public void setLength(int length) {
        this.length = length;
    }
}
