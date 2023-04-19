package com.atonement.crystals.dnr.vikari.core.operator.control.flow;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The iteration element operator <- is used in a for-each statement
 * to designate the left operand as a reference to each element in
 * the right operand.
 */
public class IterationElementOperatorCrystal extends AtonementCrystal {

    public IterationElementOperatorCrystal() {
        super(TokenType.ITERATION_ELEMENT.getIdentifier());
    }

}