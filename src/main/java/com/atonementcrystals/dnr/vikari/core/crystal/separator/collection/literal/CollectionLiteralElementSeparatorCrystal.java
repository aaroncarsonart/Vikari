package com.atonementcrystals.dnr.vikari.core.crystal.separator.collection.literal;


import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A pipe | denotes separations between elements of a collection literal.
 */
public class CollectionLiteralElementSeparatorCrystal extends AtonementCrystal {

    public CollectionLiteralElementSeparatorCrystal() {
        super(TokenType.COLLECTION_LITERAL_ELEMENT_SEPARATOR.getIdentifier());
    }

}
