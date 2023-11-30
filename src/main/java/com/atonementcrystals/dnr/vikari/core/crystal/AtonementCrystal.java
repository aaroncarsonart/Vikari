package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

/**
 * AtonementCrystals are the base type for all components of a
 * Vikari source code file. Each crystal has an associated field,
 * which holds all state of that crystal's instance in the program.
 */
public class AtonementCrystal {
    protected String identifier;
    private AtonementField field = new AtonementField();
    private CoordinatePair coordinates;

    private TypeCrystal declaredType;

    public AtonementCrystal(String identifier) {
        this.identifier = identifier;
    }

    /** Only for use in the copy() method. */
    private AtonementCrystal() {
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

    public void setCoordinates(int row, int column) {
        this.coordinates = new CoordinatePair(row, column);
    }

    public void setCoordinates(CoordinatePair coordinates) {
        this.coordinates = coordinates;
    }

    public TypeCrystal getDeclaredType() {
        return declaredType;
    }

    public void setDeclaredType(TypeCrystal declaredType) {
        this.declaredType = declaredType;
    }

    public void setDeclaredType(VikariType vikariType) {
        this.declaredType = vikariType.getTypeCrystal();
    }

    public TypeCrystal getInstantiatedType() {
        return field.getInstantiatedType();
    }

    public void setInstantiatedType(TypeCrystal instantiatedType) {
        field.setInstantiatedType(instantiatedType);
    }

    /**
     * Set the declared and instantiated types to the given type.
     * @param vikariType The type to set.
     */
    public void setType(VikariType vikariType) {
        TypeCrystal typeCrystal = vikariType.getTypeCrystal();
        setDeclaredType(typeCrystal);
        setInstantiatedType(typeCrystal);
    }

    public String getStringRepresentation() {
        return identifier;
    }

    @Override
    public String toString() {
        return identifier;
    }

    public AtonementCrystal copy() {
        AtonementCrystal copy = new AtonementCrystal();
        copyFields(this, copy);
        return copy;
    }

    public static void copyFields(AtonementCrystal original, AtonementCrystal copy) {
        copy.setIdentifier(original.getIdentifier());
        copy.setField(original.getField());
        // NOTE: A copied crystal's original coordinates will be invalid for the copy.
        // NOTE: A copied crystal's declared type will always be overridden.
        // NOTE: A copied crystal's instantiated type is located in its AtonementField.
    }

    public boolean isEqual(AtonementCrystal other) {
        return this.getField() == other.getField();
    }
}
