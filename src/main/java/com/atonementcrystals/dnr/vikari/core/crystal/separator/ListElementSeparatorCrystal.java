package com.atonementcrystals.dnr.vikari.core.crystal.separator;


import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A pipe | denotes separations between elements of a list literal.
 */
public class ListElementSeparatorCrystal extends AtonementCrystal {

    public ListElementSeparatorCrystal() {
        super(TokenType.LIST_ELEMENT_SEPARATOR.getIdentifier());
    }

}
