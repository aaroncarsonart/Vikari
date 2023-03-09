package com.atonement.crystals.dnr.vikari.core.keyword;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The conditional branch operator `??` begins a conditional branching statement.
 * (An if statement.)
 */
public class ConditionalBranchCrystal extends AtonementCrystal {

    public ConditionalBranchCrystal() {
        super(DefaultIdentifierMapping.CONDITIONAL_BRANCH.getIdentifier());
    }
}
