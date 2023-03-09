package com.atonement.crystals.dnr.vikari.core.separator.list;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * A left-parenthesis character `(` denotes the beginning of a
 * list literal constructor.
 */
public class LeftParenthesisCrystal extends AtonementCrystal {
    public LeftParenthesisCrystal() {
        super(DefaultIdentifierMapping.LEFT_PARENTHESIS.getIdentifier());
    }
}
