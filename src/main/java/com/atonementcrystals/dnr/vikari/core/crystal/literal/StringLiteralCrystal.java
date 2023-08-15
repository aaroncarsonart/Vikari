package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * String literal identifiers are any such sequence of characters
 * enclosed between two capture quotations. Strings can wrap across
 * multiple lines without the need to include an escaped newline inside
 * the string definition. Backticks within a string literal can be
 * escaped using a backslash.
 */
public class StringLiteralCrystal extends AtonementCrystal {

    private String string;

    public StringLiteralCrystal(String identifier) {
        super(identifier);
    }

    public String getString() {
        return string;
    }

    public void setString(String string) {
        this.string = string;
    }
}
