package com.atonement.crystals.dnr.vikari.core.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The delete operator deletes a crystal from the memory of the local field.
 */
public class DeleteOperatorCrystal extends AtonementCrystal {
    public DeleteOperatorCrystal() {
        super(DefaultIdentifierMapping.DELETE.getIdentifier());
    }
}
