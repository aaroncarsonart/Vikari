package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

/**
 * Represents a crystal that encapsulates all Type-related information for
 * any AtonementCrystal Type instantiable as a ReferenceCrystal. Secondary
 * references to Types should use a TypeReferenceCrystal instead.
 */
public class TypeCrystal extends AtonementCrystal {
    private final String packageName;
    private final String typeName;
    private List<TypeCrystal> children;

    public TypeCrystal(String packageName, String typeName) {
        super(typeName);
        this.packageName =  packageName;
        this.typeName = typeName;
        this.children = new ArrayList<>();
    }

    public String getPackageName() {
        return packageName;
    }

    public String getTypeName() {
        return typeName;
    }

    public String getFullyQualifiedTypeName() {
        return packageName + "::" + typeName;
    }

    public List<TypeCrystal> getChildren() {
        return children;
    }

    public void addChildren(TypeCrystal... children) {
        for (TypeCrystal child : children) {
            this.children.add(child);
        }
    }

    /**
     * Check if this TypeCrystal is within the inheritance hierarchy of the other TypeCrystal.
     * @param other A TypeCrystal that is checked to be equal to or the parent of this TypeCrystal.
     * @return True if other is equal-to or below this TypeCrystal in the inheritance hierarchy.
     */
    public boolean hasType(TypeCrystal other) {
        if (this == other) return true;
        return checkInheritance(this, other);
    }

    /**
     * Check if this TypeCrystal is within the inheritance hierarchy of the other TypeCrystal.
     * @param other The VikariType for the TypeCrystal that is checked to be equal to or the
     *              parent of this TypeCrystal.
     * @return True if the TypeCrystal pointed to by other is equal-to or below this TypeCrystal
     *         in the inheritance hierarchy.
     */
    public boolean hasType(VikariType other) {
        return hasType(other.getTypeCrystal());
    }

    /**
     * Check if the first argument is a child type of the second argument.
     * @param candidateChild The type checked to be a child of the parent.
     * @param candidateParent The type checked to be a parent of the child.
     * @return True if there is a parent-child relationship, else false.
     */
    public static boolean checkInheritance(TypeCrystal candidateChild, TypeCrystal candidateParent) {
        TypeCrystal result = walkTree(candidateParent, candidateChild::equals);
        return result != null;
    }

    private static TypeCrystal walkTree(TypeCrystal current, Predicate<TypeCrystal> predicate) {
        for (TypeCrystal child : current.children) {
            if (predicate.test(child)) {
                return child;
            }
            TypeCrystal result = walkTree(child, predicate);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    @Override
    public String toString() {
        return typeName;
    }

    /**
     * Only one instance of each unique kind of TypeCrystal exists.
     * So reference equality does the job.
     * @param obj The Object to compare equality for.
     * @return True if this TypeCrystal is equal to the other Object, else false.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof VikariType) {
            return this == ((VikariType) obj).getTypeCrystal();
        }
        return this == obj;
    }
}
