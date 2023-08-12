package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;

/**
 * Boolean crystals can be represented as literal values using the keywords of `true` or `false`.
 */
public class BooleanCrystal extends ValueCrystal<Boolean> {

    public BooleanCrystal(String identifier, String value) {
        super(identifier, value);
        setType(VikariType.BOOLEAN);
    }

    public BooleanCrystal(String identifier, Boolean value) {
        super(identifier, value);
        setType(VikariType.BOOLEAN);
    }

    public BooleanCrystal(Boolean value) {
        this(value.toString(), value);
    }

    @Override
    public ValueCrystal<Boolean> copy() {
        BooleanCrystal copy = new BooleanCrystal(getIdentifier(), getValue());
        copyFields(this, copy);
        return copy;
    }

    @Override
    public Boolean initialize(String value) {
        return Boolean.valueOf(value);
    }
}
