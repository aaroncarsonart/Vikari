package com.atonement.crystals.dnr.vikari.core.operator.logical;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The not operator ', which is a singular single-quote character and
 * not to be confused with a singular backtick `, is used to signify
 * the logical operation of NOT (value).
 */
public class LogicalNotOperatorCrystal extends AtonementCrystal {

    public LogicalNotOperatorCrystal() {
        super(DefaultIdentifierMapping.LOGICAL_NOT.getIdentifier());
    }
}
