package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

public class FloatCrystal extends NumberCrystal<Float> {
    public FloatCrystal(String identifier, String value) {
        super(identifier, value);
        setType(VikariType.FLOAT);
    }

    public FloatCrystal(String identifier, Float value) {
        super(identifier, value);
        setType(VikariType.FLOAT);
    }

    @Override
    public Float initialize(String value) {
        return Float.valueOf(value);
    }
}
