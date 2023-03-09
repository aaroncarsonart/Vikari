package com.atonement.crystals.dnr.vikari.core.operator.comparison;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class LessThanOrEqualsComparisonOperatorCrystal extends AtonementCrystal {

    public LessThanOrEqualsComparisonOperatorCrystal() {
        super(DefaultIdentifierMapping.LESS_THAN_OR_EQUALS.getIdentifier());
    }
}
