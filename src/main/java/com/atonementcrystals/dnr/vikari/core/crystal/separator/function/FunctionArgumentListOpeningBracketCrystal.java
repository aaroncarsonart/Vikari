package com.atonementcrystals.dnr.vikari.core.crystal.separator.function;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left-parenthesis ( denotes the beginning of a function call's argument list.
 */
public class FunctionArgumentListOpeningBracketCrystal extends AtonementCrystal {

    public FunctionArgumentListOpeningBracketCrystal() {
        super(TokenType.FUNCTION_ARGUMENT_LIST_OPENING_BRACKET.getIdentifier());
    }

}
