package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The feather operator `~`.
 */
public class FeatherCrystal extends AtonementCrystal {
    public FeatherCrystal() {
        super(DefaultIdentifierMapping.FEATHER.getIdentifier());
    }
}
