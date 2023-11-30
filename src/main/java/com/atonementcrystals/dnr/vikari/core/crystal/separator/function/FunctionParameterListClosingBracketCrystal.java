package com.atonementcrystals.dnr.vikari.core.crystal.separator.function;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left-parenthesis ) denotes the ending of a function declaration's parameter list.
 */
public class FunctionParameterListClosingBracketCrystal extends AtonementCrystal {

    public FunctionParameterListClosingBracketCrystal() {
        super(TokenType.FUNCTION_PARAMETER_LIST_CLOSING_BRACKET.getIdentifier());
    }

}
