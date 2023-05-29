package com.atonementcrystals.dnr.vikari.core.crystal.identifier;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * Represents an unresolved reference to any Type crystal.
 * The concrete type will be resolved later by the parser.
 */
public class TypeReferenceCrystal extends AtonementCrystal {

    public TypeReferenceCrystal(String identifier) {
        super(identifier);
    }

}
