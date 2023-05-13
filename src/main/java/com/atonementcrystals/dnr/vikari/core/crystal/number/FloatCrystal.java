package com.atonementcrystals.dnr.vikari.core.crystal.number;

public class FloatCrystal extends NumberCrystal<Float> {
    public FloatCrystal(String identifier, String value) {
        super(identifier, value);
    }

    public FloatCrystal(String identifier, Float value) {
        super(identifier, value);
    }

    @Override
    public Float initialize(String value) {
        return Float.valueOf(value);
    }
}
