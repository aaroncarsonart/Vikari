package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

public class IntegerCrystal extends NumberCrystal<Integer> {
    public IntegerCrystal(String identifier, String value) {
        super(identifier, value);
        setType(VikariType.INTEGER);
    }

    public IntegerCrystal(String identifier, Integer value) {
        super(identifier, value);
    }

    @Override
    public Integer initialize(String value) {
        return Integer.valueOf(value);
    }
}
