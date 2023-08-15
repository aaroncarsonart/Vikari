package com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The annotation operator $ followed by a pair of curly brackets { }
 * enclosing a list of operands separated by | supplies the operands as
 * constructor arguments in order to instantiate a new Annotation.
 */
public class AnnotationOperatorCrystal extends AtonementCrystal {

    public AnnotationOperatorCrystal() {
        super(TokenType.ANNOTATION.getIdentifier());
    }

}
