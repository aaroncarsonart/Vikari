package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.Keyword;

public class NullKeywordCrystal extends NullCrystal {

    public NullKeywordCrystal() {
        super(Keyword.NULL.getIdentifier(), 0);
    }

}
