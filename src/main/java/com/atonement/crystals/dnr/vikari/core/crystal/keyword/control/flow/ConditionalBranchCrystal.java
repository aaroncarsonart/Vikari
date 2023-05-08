package com.atonement.crystals.dnr.vikari.core.crystal.keyword.control.flow;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The conditional branch crystal `??` begins a conditional branching statement.
 * (An if statement.)
 */
public class ConditionalBranchCrystal extends AtonementCrystal {

    public ConditionalBranchCrystal() {
        super(TokenType.CONDITIONAL_BRANCH.getIdentifier());
    }

}
