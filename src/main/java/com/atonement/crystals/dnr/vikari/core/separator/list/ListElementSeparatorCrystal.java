package com.atonement.crystals.dnr.vikari.core.separator.list;


import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The pipe character `|` denotes separations between crystals
 * within a list constructor literal syntax.
 */
public class ListElementSeparatorCrystal extends AtonementCrystal {

    public ListElementSeparatorCrystal() {
        super(DefaultIdentifierMapping.LIST_ELEMENT_SEPARATOR.getIdentifier());
    }
}
