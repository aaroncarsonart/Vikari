package com.atonement.crystals.dnr.vikari.core.operator.angelguard;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class CatchAllCrystal extends AtonementCrystal {
    public CatchAllCrystal() {
        super(DefaultIdentifierMapping.CATCH_ALL.getIdentifier());
    }
}
