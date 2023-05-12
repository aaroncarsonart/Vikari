package com.atonement.crystals.dnr.vikari.core.crystal.number;

import java.math.BigInteger;

public class BigIntegerCrystal extends NumberCrystal<BigInteger> {
    public BigIntegerCrystal(String identifier, String value) {
        super(identifier, value);
    }

    public BigIntegerCrystal(String identifier, BigInteger value) {
        super(identifier, value);
    }

    @Override
    public BigInteger initialize(String value) {
        return new BigInteger(value);
    }
}
