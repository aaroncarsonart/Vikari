package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The variable arguments list operator ... denotes that any number
 * of arguments of the provided type may be sequentially passed to
 * that function's arguments list. These sequential arguments are
 * then rolled up into a list of that provided type.<br/>
 * <br/>
 * Multiple variable argument lists may be provided for the same
 * function signature. Given that each argument has a clearly
 * distinguishable type from all previous variable argument lists.
 */
public class VariableArgumentsListOperatorCrystal extends AtonementCrystal {

    public VariableArgumentsListOperatorCrystal() {
        super(TokenType.VARIABLE_ARGUMENTS_LIST.getIdentifier());
    }

}
