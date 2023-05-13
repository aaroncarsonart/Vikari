package com.atonementcrystals.dnr.vikari.core.crystal.number;

public class DoubleCrystal extends NumberCrystal<Double> {
    public DoubleCrystal(String identifier, String value) {
        super(identifier, value);
    }

    public DoubleCrystal(String identifier, Double value) {
        super(identifier, value);
    }

    @Override
    public Double initialize(String value) {
        return Double.valueOf(value);
    }
}
