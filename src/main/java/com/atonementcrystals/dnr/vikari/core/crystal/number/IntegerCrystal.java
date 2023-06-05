package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

public class IntegerCrystal extends NumberCrystal<Integer> {
    public IntegerCrystal(String identifier, String value) {
        super(identifier, value);
        setType(VikariType.INTEGER);
    }

    public IntegerCrystal(String identifier, Integer value) {
        super(identifier, value);
        setType(VikariType.INTEGER);
    }

    public IntegerCrystal(Integer value) {
        this(value.toString(), value);
    }

    @Override
    public IntegerCrystal copy() {
        IntegerCrystal copy = new IntegerCrystal(getIdentifier(), getValue());
        copyFields(this, copy);
        return copy;
    }

    @Override
    public Integer initialize(String value) {
        return Integer.valueOf(value);
    }
}
