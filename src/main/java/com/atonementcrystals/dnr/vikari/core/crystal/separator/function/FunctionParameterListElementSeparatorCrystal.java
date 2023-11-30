package com.atonementcrystals.dnr.vikari.core.crystal.separator.function;


import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A pipe | denotes separations between elements of a function declaration's parameter list.
 */
public class FunctionParameterListElementSeparatorCrystal extends AtonementCrystal {

    public FunctionParameterListElementSeparatorCrystal() {
        super(TokenType.LIST_ELEMENT_SEPARATOR.getIdentifier());
    }

}
