package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

import java.util.HashMap;
import java.util.Map;

/**
 * Encapsulates logic for initializing the Type hierarchy for built-in types.
 */
public class TypeHierarchy {

    /** Ensure init method is not executed multiple times for test cases. */
    private static boolean initialized = false;

    /** Map of Null types. Only instantiated on an as-needed basis. */
    private static final Map<TypeCrystal, NullTypeCrystal> nullTypes = new HashMap<>();

    /**
     * Call once before referencing type hierarchy info in TypeCrystal.
     */
    public static void init() {
        if (initialized) return;
        initialized = true;
        init(VikariType.ATONEMENT_CRYSTAL,
                init(VikariType.TYPE),
                init(VikariType.VALUE,
                        init(VikariType.BOOLEAN),
                        init(VikariType.NUMBER,
                                init(VikariType.INTEGER),
                                init(VikariType.LONG),
                                init(VikariType.BIG_INTEGER),
                                init(VikariType.FLOAT),
                                init(VikariType.DOUBLE),
                                init(VikariType.BIG_DECIMAL)
                        )
                )
        );
    }

    /**
     * Register the children with the given parent type.
     * @param parent The parent type to add the children to.
     * @param children The child types to add as children to the parent.
     * @return The parent.
     */
    private static TypeCrystal init(VikariType parent, TypeCrystal... children) {
        TypeCrystal parentTypeCrystal = parent.getTypeCrystal();
        parentTypeCrystal.addChildren(children);
        return parentTypeCrystal;
    }

    public static NullTypeCrystal getNullTypeFor(VikariType type) {
        return getNullTypeFor(type.getTypeCrystal());
    }

    public static NullTypeCrystal getNullTypeFor(TypeCrystal typeCrystal) {
        return nullTypes.computeIfAbsent(typeCrystal, t -> {
            NullTypeCrystal nullTypeCrystal = new NullTypeCrystal(t);
            nullTypeCrystal.setType(VikariType.TYPE);
            return nullTypeCrystal;
        });
    }
}
