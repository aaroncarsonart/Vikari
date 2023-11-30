package com.atonementcrystals.dnr.vikari.core.crystal.separator.function;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left-parenthesis ( denotes the beginning of a function declaration's parameter list.
 */
public class FunctionParameterListOpeningBracketCrystal extends AtonementCrystal {

    public FunctionParameterListOpeningBracketCrystal() {
        super(TokenType.FUNCTION_PARAMETER_LIST_OPENING_BRACKET.getIdentifier());
    }

}
