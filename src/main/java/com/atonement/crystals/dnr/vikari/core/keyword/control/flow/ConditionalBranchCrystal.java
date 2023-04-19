package com.atonement.crystals.dnr.vikari.core.keyword.control.flow;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The conditional branch crystal `??` begins a conditional branching statement.
 * (An if statement.)
 */
public class ConditionalBranchCrystal extends AtonementCrystal {

    public ConditionalBranchCrystal() {
        super(TokenType.CONDITIONAL_BRANCH.getIdentifier());
    }

}
