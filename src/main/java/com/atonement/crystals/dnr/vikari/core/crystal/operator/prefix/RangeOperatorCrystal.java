package com.atonement.crystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The range operator .. denotes a sequential list of numbers.
 * Defined by the inclusive range between the two integer values.
 * The range can be increasing or decreasing, based on if the smaller
 * number is provided for the left or right operand, respectively.
 * If the two operands are equal, the range has one value equal to
 * the value of the left operand.
 */
public class RangeOperatorCrystal extends AtonementCrystal {

    public RangeOperatorCrystal() {
        super(TokenType.RANGE.getIdentifier());
    }

}
