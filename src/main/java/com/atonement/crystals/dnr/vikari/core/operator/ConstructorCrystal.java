package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class ConstructorCrystal extends AtonementCrystal {

    public ConstructorCrystal() {
        super(DefaultIdentifierMapping.CONSTRUCTOR.getIdentifier());
    }
}
