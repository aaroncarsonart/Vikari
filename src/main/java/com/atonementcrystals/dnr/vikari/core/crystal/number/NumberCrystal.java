package com.atonementcrystals.dnr.vikari.core.crystal.number;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;

public abstract class NumberCrystal<V> extends ValueCrystal {
    private V value;

    public NumberCrystal(String identifier, String value) {
        super(identifier);
        this.value = initialize(value);
    }

    public NumberCrystal(String identifier, V value) {
        super(identifier);
        this.value = value;
    }

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
        // TODO: Add support for null values in Vikari.
        return "_";
    }
}
