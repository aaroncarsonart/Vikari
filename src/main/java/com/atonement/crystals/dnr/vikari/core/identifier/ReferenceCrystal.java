package com.atonement.crystals.dnr.vikari.core.identifier;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

/**
 * Represents an unresolved reference to any crystal type.
 * The concrete type will be resolved later by the parser.
 */
public class ReferenceCrystal extends AtonementCrystal {

    public ReferenceCrystal(String identifier) {
        super(identifier);
    }

}
