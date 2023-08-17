package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.NullTypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.error.Vikari_TypeException;

/**
 * A NullCrystal represents a null value in Vikari. Nulls have a length,
 * which can help to differentiate between different null instances in
 * algorithms. So in effect, a NullCrystal is also an integer.
 */
public class NullCrystal extends AtonementCrystal {
    private int length;

    public NullCrystal(String identifier, int length) {
        super(identifier);
        this.length = length;
    }

    public NullCrystal(int length) {
        this(buildIdentifier(length), length);
    }

    public int getLength() {
        return length;
    }

    @Override
    public NullCrystal copy() {
        NullCrystal copy = new NullCrystal(getIdentifier(), length);
        copyFields(this, copy);
        return copy;
    }

    private static String buildIdentifier(int length) {
        return String.format("__[%d]__", length);
    }

    @Override
    public String getStringRepresentation() {
        if (length == 0) {
            return "null";
        }
        return String.format("Null::{length=%d}", length);
    }

    @Override
    public String toString() {
        return getStringRepresentation();
    }

    @Override
    public boolean isEqual(AtonementCrystal other) {
        if (other instanceof NullKeywordCrystal) {
            return true;

        } else if (other instanceof NullCrystal otherNullCrystal) {
            if (this.getInstantiatedType() instanceof NullTypeCrystal nullType1 &&
                    other.getInstantiatedType() instanceof NullTypeCrystal nullType2) {

                TypeCrystal parentType1 = nullType1.getParent();
                TypeCrystal parentType2 = nullType2.getParent();

                // Compare the length of the two NullCrystals. But only if one of them has a parentType of Null,
                // or otherwise if one parentType inherits from the other parentType.
                return (((parentType1.isEqual(VikariType.NULL) || parentType2.isEqual(VikariType.NULL)) ||
                        (parentType1.hasType(parentType2) || parentType2.hasType(parentType1)))) &&
                        this.length == otherNullCrystal.length;

            } else {
                boolean leftOperandHasTypeError = !(this.getInstantiatedType() instanceof NullTypeCrystal);
                boolean rightOperandHasTypeError = !(other.getInstantiatedType() instanceof NullTypeCrystal);

                String errorMessage;
                if (leftOperandHasTypeError && rightOperandHasTypeError) {
                    errorMessage = "Left and right operands of equality expression have type Null, but their " +
                            "TypeCrystals don't have type NullTypeCrystal.";
                } else {
                    errorMessage = (leftOperandHasTypeError ? "Left" : "Right") + " operand of equality expression " +
                            "has type Null, but its TypeCrystal doesn't have type NullTypeCrystal.";
                }
                throw new Vikari_TypeException(errorMessage);
            }
        }
        return false;
    }
}
