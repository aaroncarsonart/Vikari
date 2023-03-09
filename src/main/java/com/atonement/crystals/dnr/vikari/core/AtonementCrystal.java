package com.atonement.crystals.dnr.vikari.core;

public class AtonementCrystal {

    private String identifier;
    private String definition;
    private AtonementField field = new AtonementField();
    private TypeCrystal type;

    public AtonementCrystal(String identifier) {
        this.identifier = identifier;
    }

    public String getIdentifier() {
        return identifier;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public String getDefinition() {
        return definition;
    }

    public void setDefinition(String definition) {
        this.definition = definition;
    }

    public AtonementField getField() {
        return field;
    }

    public void setField(AtonementField field) {
        this.field = field;
    }

    public TypeCrystal getType() {
        return type;
    }

    public void setType(TypeCrystal type) {
        this.type = type;
    }
}
