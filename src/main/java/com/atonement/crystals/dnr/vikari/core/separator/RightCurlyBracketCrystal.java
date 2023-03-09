package com.atonement.crystals.dnr.vikari.core.separator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * A left-curly-bracket character `{` denotes one-half of the
 * direction of either beginning or ending an Atonement Field
 * projection enclosure. Along with `}`, this alone determines
 * if a crystal's identifier is to be understood as being either
 * {Janspirical} or }Rapnirical{.}
 */
public class RightCurlyBracketCrystal extends AtonementCrystal {
    public RightCurlyBracketCrystal() {
        super(DefaultIdentifierMapping.RIGHT_CURLY_BRACKET.getIdentifier());
    }
}
