package com.atonement.crystals.dnr.vikari.core.keyword.error;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The catch identifier `++` catches an explicitly identified Vikari.
 */
public class CatchCrystal extends AtonementCrystal {
    public CatchCrystal() {
        super(DefaultIdentifierMapping.CATCH.getIdentifier());
    }
}
