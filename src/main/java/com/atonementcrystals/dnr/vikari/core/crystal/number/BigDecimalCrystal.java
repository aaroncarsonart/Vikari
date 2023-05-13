package com.atonementcrystals.dnr.vikari.core.crystal.number;

import java.math.BigDecimal;

public class BigDecimalCrystal extends NumberCrystal<BigDecimal> {
    public BigDecimalCrystal(String identifier, String value) {
        super(identifier, value);
    }

    public BigDecimalCrystal(String identifier, BigDecimal value) {
        super(identifier, value);
    }

    @Override
    public BigDecimal initialize(String value) {
        return new BigDecimal(value);
    }
}
