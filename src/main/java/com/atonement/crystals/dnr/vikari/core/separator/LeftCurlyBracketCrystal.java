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
public class LeftCurlyBracketCrystal extends AtonementCrystal {
    public LeftCurlyBracketCrystal() {
        super(DefaultIdentifierMapping.LEFT_CURLY_BRACKET.getIdentifier());
    }
}
