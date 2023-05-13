package com.atonementcrystals.dnr.vikari.core.crystal.separator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A comma , is a statement separator. This denotes separation
 * between a sequential series of statements when written on the
 * same line.<br/>
 * <br/>
 * For indented code, these are not necessary to terminate ordinary
 * single statements. But they can be used optionally to explicitly
 * terminate every code statement.<br/>
 * <br/>
 * However, these are necessary for minimized Vikari code. Wherein
 * they are used in conjuction with the region separator ; to
 * explicitly terminate every minimized Vikari code statement.
 */
public class StatementSeparatorCrystal extends AtonementCrystal {

    public StatementSeparatorCrystal() {
        super(TokenType.STATEMENT_SEPARATOR.getIdentifier());
    }

}
