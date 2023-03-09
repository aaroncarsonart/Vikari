package com.atonement.crystals.dnr.vikari.core.separator.list;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * A right-parenthesis character `)` denotes the beginning of a
 * list literal constructor.
 */
public class RightParenthesisCrystal extends AtonementCrystal {
    public RightParenthesisCrystal() {
        super(DefaultIdentifierMapping.RIGHT_PARENTHESIS.getIdentifier());
    }
}
