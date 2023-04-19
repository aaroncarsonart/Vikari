package com.atonement.crystals.dnr.vikari.core.keyword.control.flow;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The loop crystal `<>` begins all loop flow control constructs.
 */
public class LoopCrystal extends AtonementCrystal {

    public LoopCrystal() {
        super(TokenType.LOOP.getIdentifier());
    }

}
