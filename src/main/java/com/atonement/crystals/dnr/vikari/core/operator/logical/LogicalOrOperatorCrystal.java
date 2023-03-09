package com.atonement.crystals.dnr.vikari.core.operator.logical;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class LogicalOrOperatorCrystal extends AtonementCrystal {

    public LogicalOrOperatorCrystal() {
        super(DefaultIdentifierMapping.LOGICAL_OR.getIdentifier());
    }
}
