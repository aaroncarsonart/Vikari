package com.atonement.crystals.dnr.vikari.core.separator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

/**
 * A whitespace crystal is composed of any number of valid whitespace
 * characters separating any two identifiers on a given line. So it
 * can be defined as any mix between tabs and spaces.
 */
public class WhitespaceCrystal extends AtonementCrystal {
    public WhitespaceCrystal(String identifier) {
        super(identifier);
    }
}