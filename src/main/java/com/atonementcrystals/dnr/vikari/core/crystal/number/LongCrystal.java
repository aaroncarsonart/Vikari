package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

public class LongCrystal extends NumberCrystal<Long> {
    public LongCrystal(String identifier, String value) {
        super(identifier, value);
        setType(VikariType.LONG);
    }

    public LongCrystal(String identifier, Long value) {
        super(identifier, value);
    }

    @Override
    public Long initialize(String value) {
        return Long.valueOf(value);
    }
}
