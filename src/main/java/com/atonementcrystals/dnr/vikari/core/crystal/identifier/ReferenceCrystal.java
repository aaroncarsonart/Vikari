package com.atonementcrystals.dnr.vikari.core.crystal.identifier;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * Represents an unresolved reference to any crystal type.
 * The concrete type will be resolved later by the parser.
 */
public class ReferenceCrystal extends AtonementCrystal {

    public ReferenceCrystal(String identifier) {
        super(identifier);
    }

}
