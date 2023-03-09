package com.atonement.crystals.dnr.vikari.core.separator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * A comma character , denotes a separation between two sets of distinct
 * Vikari code statements. This enables multiple statements to be written
 * sequentially on the same line. As well as to provide an optional line-
 * terminator crystal for ending statements that may otherwise read as
 * potentially ambiguous to the compiler.
 */
public class MultipleStatementSeparatorCrystal extends AtonementCrystal {

    public MultipleStatementSeparatorCrystal() {
        super(DefaultIdentifierMapping.MULTIPLE_STATEMENT_SEPARATOR.getIdentifier());
    }
}
