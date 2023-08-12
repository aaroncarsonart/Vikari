package com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.interpreter.NumericComparisons;

/**
 * The not equals operator '= checks if the left operand is equal
 * to the right operand by calling <code>'left.equals!(right)</code>
 * as a null-safe operation.
 */
public class NotEqualsOperatorCrystal extends BinaryOperatorCrystal {

    public NotEqualsOperatorCrystal() {
        super(TokenType.NOT_EQUALS.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        if (left instanceof NumberCrystal<?> leftNumber && right instanceof NumberCrystal<?> rightNumber) {
            return NumericComparisons.isNotEqual(leftNumber, rightNumber);
        } else {
            BooleanCrystal result = new BooleanCrystal(!left.isEqual(right));
            result.setCoordinates(left.getCoordinates());
            return result;
        }
    }
}
