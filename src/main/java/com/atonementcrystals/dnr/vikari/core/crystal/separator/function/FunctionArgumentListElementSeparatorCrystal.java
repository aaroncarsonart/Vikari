package com.atonementcrystals.dnr.vikari.core.crystal.separator.function;


import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A pipe | denotes separations between elements of a function call's argument list.
 */
public class FunctionArgumentListElementSeparatorCrystal extends AtonementCrystal {

    public FunctionArgumentListElementSeparatorCrystal() {
        super(TokenType.LIST_ELEMENT_SEPARATOR.getIdentifier());
    }

}
