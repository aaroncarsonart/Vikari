package com.atonement.crystals.dnr.vikari.core.literal;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

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
