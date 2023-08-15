package com.atonementcrystals.dnr.vikari.core.crystal.operator.logical;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

/**
 * The logical OR operator " applies the logical OR operation to
 * the left and right operands. This performs short-circuiting, so
 * the second operand is only evaluated if the left operand evaluates
 * to false.
 */
public class LogicalOrOperatorCrystal extends BinaryOperatorCrystal {

    public LogicalOrOperatorCrystal() {
        super(TokenType.LOGICAL_OR.getIdentifier());
    }

    @Override
    public AtonementCrystal evaluate(AtonementCrystal left, AtonementCrystal right) {
        throw new UnsupportedOperationException("Binary logical operators are not evaluated using the evaluate() method.");
    }

}
