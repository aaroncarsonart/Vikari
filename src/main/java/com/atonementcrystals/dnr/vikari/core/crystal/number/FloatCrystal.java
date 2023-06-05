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

    public FloatCrystal(Float value) {
        this(value.toString(), value);
    }

    @Override
    public FloatCrystal copy() {
        FloatCrystal copy = new FloatCrystal(getIdentifier(), getValue());
        copyFields(this, copy);
        return copy;
    }

    @Override
    public Float initialize(String value) {
        return Float.valueOf(value);
    }
}
