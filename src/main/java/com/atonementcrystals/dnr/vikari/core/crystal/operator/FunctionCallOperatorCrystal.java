package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The function call operator ! applies a list literal containing the
 * function arguments from the right operand to the function
 * reference returned by the left operand, and then calls the function
 * with those arguments. A function call with no arguments may elide
 * the empty parentheses ().<br/>
 * <pre>
 * foo!(bar|baz)
 * foo!()
 * foo!</pre>
 *
 * When the function call operator is applied to a type name, this
 * calls that type's constructor to instantiate a new instance of that
 * type. This can be combined with a variable declaration's type label
 * such that the << operator can be elided.<br/>
 * <pre>
 * foo:Bar << Bar!(baz)
 * foo:Bar!(baz)</pre>
 *
 * Untyped variable declarations must use the assignment syntax form.
 * <pre>
 * foo << Bar!(baz)</pre>
 */
public class FunctionCallOperatorCrystal extends AtonementCrystal {

    public FunctionCallOperatorCrystal() {
        super(TokenType.FUNCTION_CALL.getIdentifier());
    }

}
