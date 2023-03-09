package com.atonement.crystals.dnr.vikari.core.keyword.label;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The `private` access modifier keyword in conjunction with `::` produces
 * a label after which all private members of a crystal's field are then
 * sequentially defined.
 */
public class PrivateAccessModifierCrystal extends AtonementCrystal {
    public PrivateAccessModifierCrystal() {
        super(DefaultIdentifierMapping.PRIVATE_ACCESS_MODIFIER.getIdentifier());
    }
}
