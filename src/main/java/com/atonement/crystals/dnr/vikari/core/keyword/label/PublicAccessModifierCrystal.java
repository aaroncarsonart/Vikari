package com.atonement.crystals.dnr.vikari.core.keyword.label;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The `public` access modifier keyword in conjunction with `::` produces
 * a label after which all public members of a crystal's field are then
 * sequentially defined.
 */
public class PublicAccessModifierCrystal extends AtonementCrystal {
    public PublicAccessModifierCrystal() {
        super(DefaultIdentifierMapping.PUBLIC_ACCESS_MODIFIER.getIdentifier());
    }
}
