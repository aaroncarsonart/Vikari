package com.atonementcrystals.dnr.vikari.core.crystal.separator.function;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left-parenthesis ) denotes the ending of a function call's argument list.
 */
public class FunctionArgumentListClosingBracketCrystal extends AtonementCrystal {

    public FunctionArgumentListClosingBracketCrystal() {
        super(TokenType.FUNCTION_ARGUMENT_LIST_CLOSING_BRACKET.getIdentifier());
    }

}
