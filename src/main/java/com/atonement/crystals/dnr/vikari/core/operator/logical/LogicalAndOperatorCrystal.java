package com.atonement.crystals.dnr.vikari.core.operator.logical;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The logical AND operator ", which is a singular double-quote character
 * and not to be confused with a capture quotation ``, is used to signify
 * the unary logical operation of NOT (value).
 */
public class LogicalAndOperatorCrystal extends AtonementCrystal {

    public LogicalAndOperatorCrystal() {
        super(DefaultIdentifierMapping.LOGICAL_AND.getIdentifier());
    }
}
