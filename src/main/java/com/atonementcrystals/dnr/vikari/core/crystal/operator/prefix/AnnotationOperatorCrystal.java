package com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The annotation operator $: followed by a type name and an optional
 * list of operands enclosed in curly brackets { } and separated by
 * the list element separator | constructs a new Annotation using the
 * supplied operands.<br/>
 * <pre>
 * $:CastTo{Integer}</pre>
 *
 * This is distinguished between the set literal constructor, i.e.
 * <code>$:{1|2|3}</code> in that a set literal does not contain a
 * typename between the <code>$:</code> and <code>{args}</code>.
 */
public class AnnotationOperatorCrystal extends AtonementCrystal {

    public AnnotationOperatorCrystal() {
        super(TokenType.ANNOTATION.getIdentifier());
    }

}
