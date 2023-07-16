package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

public abstract class NumberCrystal<V> extends ValueCrystal<V> {

    /**
     * This location is cached here so this operator, if present, can be
     * syntax-highlighted properly by editors which use the Lexer output.
     */
    private CoordinatePair negationOperatorLocation;

    public NumberCrystal(String identifier, String value) {
        super(identifier, value);
    }

    public NumberCrystal(String identifier, V value) {
        super(identifier, value);
    }

    public CoordinatePair getNegationOperatorLocation() {
        return negationOperatorLocation;
    }

    public void setNegationOperatorLocation(CoordinatePair negationOperatorLocation) {
        this.negationOperatorLocation = negationOperatorLocation;
    }
}
