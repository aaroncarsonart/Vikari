package com.atonement.crystals.dnr.vikari.core.operator.math;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The ADD operator `+` adds two numerical values together.
 */
public class AddOperatorCrystal extends AtonementCrystal {
    public AddOperatorCrystal() {
        super(DefaultIdentifierMapping.ADD.getIdentifier());
    }
}
