package com.atonement.crystals.dnr.vikari.core.keyword;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The loop operator `<>` in conjunction with a boolean clause, `::`,
 * and a series of statements in a new enclosed or open scope begin
 * a for or while statement.
 */
public class LoopCrystal extends AtonementCrystal {

    public LoopCrystal() {
        super(DefaultIdentifierMapping.LOOP.getIdentifier());
    }
}
