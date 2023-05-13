package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.util.Utils;

/**
 * Encapsulates all type-related information for an AtonementCrystal.
 */
public class Type {
    private final String name;
    private final String packageName;

    public Type(String packageName, String name) {
        Utils.validateFullyQualifiedTypeName(packageName + "::" + name);
        this.name = name;
        this.packageName = packageName;
    }

    public String getName() {
        return name;
    }

    public String getPackageName() {
        return packageName;
    }

    public String getFullyQualifiedTypeName() {
        return packageName + "::" + name;
    }
}
