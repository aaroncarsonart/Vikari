package com.atonement.crystals.dnr.vikari.core;

import com.atonement.crystals.dnr.vikari.util.CoordinatePair;

/**
 * AtonementCrystals are the base type for all components of a
 * Vikari source code file. Each crystal has an associated field,
 * which holds all state of that crystal's instance in the program.
 */
public class AtonementCrystal {
    private String identifier;
    private AtonementField field = new AtonementField();
    private CoordinatePair coordinates;

    public AtonementCrystal(String identifier) {
        this.identifier = identifier;
    }

    public String getIdentifier() {
        return identifier;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public AtonementField getField() {
        return field;
    }

    public void setField(AtonementField field) {
        this.field = field;
    }

    public CoordinatePair getCoordinates() {
        return coordinates;
    }

    public void setCoordinates(CoordinatePair coordinates) {
        this.coordinates = coordinates;
    }

    public void setCoordinates(int row, int column) {
        this.coordinates = new CoordinatePair(row, column);
    }
}
