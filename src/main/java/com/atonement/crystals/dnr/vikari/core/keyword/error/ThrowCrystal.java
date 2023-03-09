package com.atonement.crystals.dnr.vikari.core.keyword.error;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The throw identifier `--` throws a new Vikari.
 */
public class ThrowCrystal extends AtonementCrystal {
    public ThrowCrystal() {
        super(DefaultIdentifierMapping.THROW.getIdentifier());
    }
}
