package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The dot operator dereferences values from a crystal's Atonement Field
 * in order to give you direct access to that field's namespace.
 */
public class DotOperatorCrystal extends AtonementCrystal {

    public DotOperatorCrystal() {
        super(DefaultIdentifierMapping.DOT_OPERATOR.getIdentifier());
    }
}

