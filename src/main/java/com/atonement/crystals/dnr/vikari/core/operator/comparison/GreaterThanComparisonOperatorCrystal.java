package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class GreaterThanComparisonOperatorCrystal extends AtonementCrystal {

    public GreaterThanComparisonOperatorCrystal() {
        super(DefaultIdentifierMapping.GREATER_THAN.getIdentifier());
    }
}
