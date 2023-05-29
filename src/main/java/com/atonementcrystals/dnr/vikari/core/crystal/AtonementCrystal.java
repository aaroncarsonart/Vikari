package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

/**
 * AtonementCrystals are the base type for all components of a
 * Vikari source code file. Each crystal has an associated field,
 * which holds all state of that crystal's instance in the program.
 */
public class AtonementCrystal {
    private String identifier;
    private AtonementField field = new AtonementField();
    private CoordinatePair coordinates;

    private TypeCrystal declaredType;
    private TypeCrystal instantiatedType;

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

    public TypeCrystal getDeclaredType() {
        return declaredType;
    }

    public void setDeclaredType(TypeCrystal declaredType) {
        this.declaredType = declaredType;
    }

    public void setDeclaredType(VikariType vikariType) {
        TypeCrystal declaredType = vikariType.getTypeCrystal();
        this.declaredType = declaredType;
    }

    public TypeCrystal getInstantiatedType() {
        return instantiatedType;
    }

    public void setInstantiatedType(TypeCrystal instantiatedType) {
        this.instantiatedType = instantiatedType;
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
}
