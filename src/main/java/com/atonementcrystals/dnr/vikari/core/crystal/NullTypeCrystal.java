package com.atonementcrystals.dnr.vikari.core.crystal;

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
