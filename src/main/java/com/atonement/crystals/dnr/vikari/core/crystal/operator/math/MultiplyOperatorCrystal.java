package com.atonement.crystals.dnr.vikari.core.crystal.operator.math;

import com.atonement.crystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The multiply operator * multiplies the left operand by the right
 * operand.<br/>
 * <br/>
 * A string or list may be multiplied by a number n to return a new
 * string or list which has repeated the original string or list n
 * times.
 */
public class MultiplyOperatorCrystal extends BinaryOperatorCrystal {

    public MultiplyOperatorCrystal() {
        super(TokenType.MULTIPLY.getIdentifier());
    }

}
