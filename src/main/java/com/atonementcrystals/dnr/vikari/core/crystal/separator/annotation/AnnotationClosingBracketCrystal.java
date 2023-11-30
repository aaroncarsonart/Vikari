package com.atonementcrystals.dnr.vikari.core.crystal.separator.annotation;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A right curly bracket } denotes the ending of an annotation constructor's argument list.
 */
public class AnnotationClosingBracketCrystal extends AtonementCrystal {

    public AnnotationClosingBracketCrystal() {
        super(TokenType.ANNOTATION_CLOSING_BRACKET.getIdentifier());
    }

}
