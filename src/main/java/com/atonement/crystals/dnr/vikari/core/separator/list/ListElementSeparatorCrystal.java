package com.atonement.crystals.dnr.vikari.core.separator.list;


import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * A pipe | denotes separations between elements of a list literal.
 */
public class ListElementSeparatorCrystal extends AtonementCrystal {

    public ListElementSeparatorCrystal() {
        super(TokenType.LIST_ELEMENT_SEPARATOR.getIdentifier());
    }

}
