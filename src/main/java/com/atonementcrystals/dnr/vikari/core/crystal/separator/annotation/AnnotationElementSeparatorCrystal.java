package com.atonementcrystals.dnr.vikari.core.crystal.separator.annotation;


import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A pipe | denotes separations between elements of an annotation's constructor's list of arguments.
 */
public class AnnotationElementSeparatorCrystal extends AtonementCrystal {

    public AnnotationElementSeparatorCrystal() {
        super(TokenType.ANNOTATION_ELEMENT_SEPARATOR.getIdentifier());
    }

}
