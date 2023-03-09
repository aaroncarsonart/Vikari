package com.atonement.crystals.dnr.vikari.core.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class CopyConstructorCrystal extends AtonementCrystal {
    public CopyConstructorCrystal() {
        super(DefaultIdentifierMapping.COPY_CONSTRUCTOR.getIdentifier());
    }
}
