package com.atonement.crystals.dnr.vikari.core.literal;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

/**
 * String literal identifiers are any such sequence of characters
 * immediately inclosed between two capture quotations. Strings
 * can naturally become wrapped across multiple lines without the
 * need to include an escaped newline inside the string definition.
 * Nested backticks can be escaped using an enclosure of square brackets.
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
