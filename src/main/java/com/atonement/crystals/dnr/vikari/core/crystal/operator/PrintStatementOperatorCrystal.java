package com.atonement.crystals.dnr.vikari.core.crystal.operator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The print statement operator : begins a print statement. A series
 * of proceeding expressions delimited by : forms a series of print
 * expressions. Each prints the string values for each expression to
 * standard output.<br/>
 * <br/>
 * An empty : at the end of a line always prints a newline.<br/>
 * <br/>
 * Flexible expressions can be passed as a previous argument to a
 * print expression in order to format further print expressions.
 */
public class PrintStatementOperatorCrystal extends AtonementCrystal {

    public PrintStatementOperatorCrystal() {
        super(TokenType.PRINT_STATEMENT.getIdentifier());
    }

}
