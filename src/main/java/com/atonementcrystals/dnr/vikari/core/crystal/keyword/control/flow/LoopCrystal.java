package com.atonementcrystals.dnr.vikari.core.crystal.keyword.control.flow;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The loop crystal <> begins all loop flow control constructs.
 */
public class LoopCrystal extends AtonementCrystal {

    public LoopCrystal() {
        super(TokenType.LOOP.getIdentifier());
    }

}
