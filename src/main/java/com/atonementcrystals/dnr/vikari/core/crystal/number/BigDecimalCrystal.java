package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

import java.math.BigDecimal;

public class BigDecimalCrystal extends NumberCrystal<BigDecimal> {
    public BigDecimalCrystal(String identifier, String value) {
        super(identifier, value);
        setType(VikariType.BIG_DECIMAL);
    }

    public BigDecimalCrystal(String identifier, BigDecimal value) {
        super(identifier, value);
        setType(VikariType.BIG_DECIMAL);
    }

    public BigDecimalCrystal(BigDecimal value) {
        this(value.toString(), value);
    }

    @Override
    public BigDecimalCrystal copy() {
        BigDecimalCrystal copy = new BigDecimalCrystal(getIdentifier(), getValue());
        copyFields(this, copy);
        return copy;
    }

    @Override
    public BigDecimal initialize(String value) {
        return new BigDecimal(value);
    }
}
