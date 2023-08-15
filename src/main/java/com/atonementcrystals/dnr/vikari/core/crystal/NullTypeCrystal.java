package com.atonementcrystals.dnr.vikari.core.crystal;

/**
 * Represents the type of any crystal which has been assigned a Null value. Technically,
 * Null is a subtype of every other type. But rather than pollute the TypeHierarchy with
 * an ordinary TypeCrystal instance for each separate Null type, a NullTypeCrystal is only
 * instantiated for a given Type if a Null value of that type is instantiated for a given
 * program. With special handling to model the expected inheritance hierarchy behavior.
 */
public class NullTypeCrystal extends TypeCrystal {
    public static final String NULL_TYPE_NAME = "Null";

    /**
     * NullTypeCrystals hold a reference to the parent type the Null is representative of.
     */
    private final TypeCrystal parent;

    public NullTypeCrystal(TypeCrystal parent) {
        super(parent.getPackageName(), NULL_TYPE_NAME);
        this.parent = parent;
    }

    public TypeCrystal getParent() {
        return parent;
    }

    @Override
    public String toString() {
        return String.format("%s[%s]", getTypeName(), parent.getTypeName());
    }
}
