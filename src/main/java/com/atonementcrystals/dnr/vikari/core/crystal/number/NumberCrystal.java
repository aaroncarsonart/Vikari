package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

public abstract class NumberCrystal<V> extends ValueCrystal {
    private V value;

    /**
     * This location is cached here so this operator, if present, can be
     * syntax-highlighted properly by editors which use the Lexer output.
     */
    private CoordinatePair negationOperatorLocation;

    public NumberCrystal(String identifier, String value) {
        super(identifier);
        this.value = initialize(value);
    }

    public NumberCrystal(String identifier, V value) {
        super(identifier);
        this.value = value;
    }

    @Override
    public abstract NumberCrystal<V> copy();

    public V getValue() {
        return value;
    }

    public void setValue(V value) {
        this.value = value;
    }

    public abstract V initialize(String value);

    @Override
    public String getStringRepresentation() {
        if (value != null) {
            return value.toString();
        }

        throw new IllegalStateException("A NumberCrystal's value cannot be null.");
    }

    public CoordinatePair getNegationOperatorLocation() {
        return negationOperatorLocation;
    }

    public void setNegationOperatorLocation(CoordinatePair negationOperatorLocation) {
        this.negationOperatorLocation = negationOperatorLocation;
    }
}
