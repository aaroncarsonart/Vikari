package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.error.Vikari_AtonementFieldException;
import com.atonementcrystals.dnr.vikari.error.Vikari_FieldMemberExistsException;
import com.atonementcrystals.dnr.vikari.error.Vikari_UndefinedFieldMemberException;

import java.util.HashMap;
import java.util.Map;

/**
 * An AtonementField describes the structure of an AtonementCrystal.
 * All type members are contained within regions of its associated
 * field. Two identifiers which point to the same crystal instance
 * share the same AtonementField.
 * <p>
 * AtonementFields are also used to model environments in which
 * program variables exist. So the main program has a global field,
 * which encloses each file's root environment. Then each new scope
 * introduced produces a new environment, which is linked to the
 * outer enclosing environment with a parent reference.
 */
public class AtonementField {
    private Map<String, AtonementCrystal> fieldMembers;
    private AtonementField parentField;

    /**
     * If true, this field can define a crystal with an identifier
     * which has already been defined in a parent field. This enables
     * fields for certain code blocks such as loops or conditional
     * statements to not allow shadowing of variable names. But other
     * code blocks such as for function bodies or type definitions
     * do shadow variable names.
     * <p>
     * By default, AtonementFields shadow variables.
     */
    private boolean shadowVariables;

    /**
     * The concrete, instantiated type of all crystals holding a
     * reference to this AtonementField.
     */
    private TypeCrystal instantiatedType;

    public AtonementField() {
        this.fieldMembers = new HashMap<>();
    }

    public AtonementField(AtonementField parentField) {
        this(parentField, true);
    }

    public AtonementField(AtonementField parentField, boolean shadowVariables) {
        this.fieldMembers = new HashMap<>();
        this.parentField = parentField;
        this.shadowVariables = shadowVariables;
    }

    public TypeCrystal getInstantiatedType() {
        return instantiatedType;
    }

    public void setInstantiatedType(TypeCrystal instantiatedType) {
        this.instantiatedType = instantiatedType;
    }

    public AtonementCrystal get(String identifier) {
        if (hasFieldMember(identifier)) {
            return fieldMembers.get(identifier);
        } else if (parentField != null) {
            return parentField.get(identifier);
        }
        throw undefinedFieldMemberError();
    }

    /**
     * Check if a crystal with the given identifier is defined in either
     * this field and any parent fields.
     * @param identifier The crystal identifier to check.
     * @return True if this field contains a reference, else null.
     */
    public boolean isDefined(String identifier) {
        AtonementField field = this;
        do {
            if (field.hasFieldMember(identifier)) {
                return true;
            }
            field = field.parentField;
        } while(!shadowVariables && field != null);
        return false;
    }

    /**
     * Get the AtonementField containing the definition for the specified crystal.
     * @param identifier The crystal identifier to check.
     * @return The AtonementField containing the definition for the specified crystal, or else null.
     */
    public AtonementField getFieldWithDefinition(String identifier) {
        AtonementField field = this;
        do {
            if (field.hasFieldMember(identifier)) {
                return field;
            }
            field = field.parentField;
        } while(!shadowVariables && field != null);
        return null;
    }

    /**
     * Check if a crystal with the given identifier is defined in only
     * this field. Does not check any parent fields.
     * @param identifier The crystal identifier to check.
     * @return True if this crystal is defined, else null.
     */
    public boolean hasFieldMember(String identifier) {
        return fieldMembers.containsKey(identifier);
    }

    /**
     * Defines a new crystal in this field. Throws an error if a crystal
     * has already been defined with the same identifier.
     * @param identifier The identifier to define a new value for.
     * @param crystal The new value to define the identifier with.

     */
    public void define(String identifier, AtonementCrystal crystal) {
        if (!isDefined(identifier)) {
            fieldMembers.put(identifier, crystal);
        } else {
            throw fieldMemberExistsError();
        }
    }

    /**
     * Assign a new value to an existing crystal definition. Throws an error
     * if the crystal has not already been defined in the field hierarchy.
     * @param identifier The identifier to assign a new value to.
     * @param crystal The new value to assign to the identifier.
     */
    public void assign(String identifier, AtonementCrystal crystal) {
        AtonementField fieldWithDefinition = getFieldWithDefinition(identifier);
        boolean isDefined = fieldWithDefinition != null;
        if (isDefined) {
            fieldWithDefinition.fieldMembers.put(identifier, crystal);
        } else {
            throw undefinedFieldMemberError();
        }
    }

    private Vikari_AtonementFieldException undefinedFieldMemberError() {
        return new Vikari_UndefinedFieldMemberException();
    }

    private Vikari_AtonementFieldException fieldMemberExistsError() {
        return new Vikari_FieldMemberExistsException();
    }
}
