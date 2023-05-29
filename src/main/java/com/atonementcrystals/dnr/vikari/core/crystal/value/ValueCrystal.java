package com.atonementcrystals.dnr.vikari.core.crystal.value;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

/**
 * This type should never be instantiated. But a basic implementation
 * is provided for clarity to ensure modeling of the type hierarchy is
 * consistent.
 */
public class ValueCrystal extends AtonementCrystal {

    // TODO: Move <V> value implementation from NumberCrystal to ValueCrystal
    //       when implementing other value types. (Boolean, Character, Byte, etc.)

    public ValueCrystal(String identifier) {
        super(identifier);
        setType(VikariType.VALUE);
    }

}
