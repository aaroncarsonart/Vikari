package com.atonement.crystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The annotation operator $ followed by a pair of curly brackets { }
 * accepts a list of operands separated by | to be applied in order
 * to instantiate a new Annotation.
 */
public class AnnotationOperatorCrystal extends AtonementCrystal {

    public AnnotationOperatorCrystal() {
        super(TokenType.ANNOTATION.getIdentifier());
    }

}
