package com.atonementcrystals.dnr.vikari.core.crystal.value;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * This type should never be instantiated. But a basic implementation
 * is provided to ensure modeling of the type hierarchy is consistent.
 */
public abstract class ValueCrystal<V> extends AtonementCrystal {
    private V value;

    public ValueCrystal(String identifier, String value) {
        super(identifier);
        this.value = initialize(value);
    }

    public ValueCrystal(String identifier, V value) {
        super(identifier);
        this.value = value;
    }

    @Override
    public abstract ValueCrystal<V> copy();

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
        throw new IllegalStateException("A ValueCrystal's value cannot be null.");
    }

    @Override
    public boolean isEqual(AtonementCrystal other) {
        if (this.getField() == other.getField()) {
            return true;
        } else if (other instanceof ValueCrystal<?> valueCrystal) {
            return this.value.equals(valueCrystal.value);
        }
        return false;
    }
}
