package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.MultilineToken;

/**
 * A multi-line string literal is a string defined across more than one line.
 */
public class MultiLineStringLiteralCrystal extends AtonementCrystal implements MultilineToken {

    private String string;
    private MultiLineStringLiteralCrystal next;
    private boolean isOpeningToken;
    private boolean isClosingToken;

    public MultiLineStringLiteralCrystal(String identifier) {
        super(identifier);
    }

    public String getString() {
        return string;
    }

    public void setString(String string) {
        this.string = string;
    }

    public MultiLineStringLiteralCrystal getNext() {
        return next;
    }

    public void setNext(MultiLineStringLiteralCrystal next) {
        this.next = next;
    }

    @Override
    public boolean isOpeningToken() {
        return isOpeningToken;
    }

    public void setOpeningToken(boolean openingToken) {
        isOpeningToken = openingToken;
    }

    @Override
    public boolean isClosingToken() {
        return isClosingToken;
    }

    public void setClosingToken(boolean closingToken) {
        isClosingToken = closingToken;
    }
}
