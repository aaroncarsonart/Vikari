package com.atonementcrystals.dnr.vikari.core.crystal.identifier;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * Represents an unresolved reference to any crystal instance.
 * The concrete type will be resolved later by the parser.
 */
public class ReferenceCrystal extends AtonementCrystal {

    public ReferenceCrystal(String identifier) {
        super(identifier);
    }

    @SuppressWarnings("unused")
    public boolean isQuotedIdentifier() {
        return identifier.length() >= 4 &&
                identifier.charAt(0) == '`' &&
                identifier.charAt(identifier.length() - 1) == '`';
    }
}
