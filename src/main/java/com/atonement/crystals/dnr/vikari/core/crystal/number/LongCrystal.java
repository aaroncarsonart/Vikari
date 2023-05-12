package com.atonement.crystals.dnr.vikari.core.crystal.number;

public class LongCrystal extends NumberCrystal<Long> {
    public LongCrystal(String identifier, String value) {
        super(identifier, value);
    }

    public LongCrystal(String identifier, Long value) {
        super(identifier, value);
    }

    @Override
    public Long initialize(String value) {
        return Long.valueOf(value);
    }
}
