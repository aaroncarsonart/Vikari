package com.atonementcrystals.dnr.vikari.core.crystal.separator.annotation;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A left curly bracket { denotes the beginning of an annotation constructor's argument list.
 */
public class AnnotationOpeningBracketCrystal extends AtonementCrystal {

    public AnnotationOpeningBracketCrystal() {
        super(TokenType.ANNOTATION_OPENING_BRACKET.getIdentifier());
    }

}
