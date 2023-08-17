package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.Keyword;

public class NullKeywordCrystal extends NullCrystal {

    public NullKeywordCrystal() {
        super(Keyword.NULL.getIdentifier(), 0);
    }

    @Override
    public boolean isEqual(AtonementCrystal other) {
        return other instanceof NullCrystal;
    }
}
