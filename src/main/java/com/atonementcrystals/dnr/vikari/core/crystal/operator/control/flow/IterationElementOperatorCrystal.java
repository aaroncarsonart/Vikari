package com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

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