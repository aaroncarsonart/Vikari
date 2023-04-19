package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The function call operator ! applies a list literal containing the
 * function arguments from the right operand to the function
 * reference returned by the left operand, and then calls the function
 * with those arguments. A function with no arguments may elide the
 * empty parentheses ().
 */
public class FunctionCallOperatorCrystal extends AtonementCrystal {

    public FunctionCallOperatorCrystal() {
        super(TokenType.FUNCTION_CALL.getIdentifier());
    }

}
