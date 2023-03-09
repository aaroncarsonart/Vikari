package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class EqualsComparisonOperatorCrystal extends AtonementCrystal {

    public EqualsComparisonOperatorCrystal() {
        super(DefaultIdentifierMapping.LOGICAL_AND.getIdentifier());
    }
}

