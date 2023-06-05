package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

import java.math.BigInteger;

public class BigIntegerCrystal extends NumberCrystal<BigInteger> {
    public BigIntegerCrystal(String identifier, String value) {
        super(identifier, value);
        setType(VikariType.BIG_INTEGER);
    }

    public BigIntegerCrystal(String identifier, BigInteger value) {
        super(identifier, value);
        setType(VikariType.BIG_INTEGER);
    }

    public BigIntegerCrystal(BigInteger value) {
        this(value.toString(), value);
    }

    @Override
    public BigIntegerCrystal copy() {
        BigIntegerCrystal copy = new BigIntegerCrystal(getIdentifier(), getValue());
        copyFields(this, copy);
        return copy;
    }

    @Override
    public BigInteger initialize(String value) {
        return new BigInteger(value);
    }
}
