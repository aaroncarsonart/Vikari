package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class LessThanComparisonOperatorCrystal extends AtonementCrystal {

    public LessThanComparisonOperatorCrystal() {
        super(DefaultIdentifierMapping.GREATER_THAN_OR_EQUALS.getIdentifier());
    }
}
