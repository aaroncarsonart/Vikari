package com.atonementcrystals.dnr.vikari.core.crystal.number;

public class IntegerCrystal extends NumberCrystal<Integer> {
    public IntegerCrystal(String identifier, String value) {
        super(identifier, value);
    }

    public IntegerCrystal(String identifier, Integer value) {
        super(identifier, value);
    }

    @Override
    public Integer initialize(String value) {
        return Integer.valueOf(value);
    }
}
