package com.atonementcrystals.dnr.vikari.core.crystal.separator.quotation;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A single backtick ` is used for quoting character literals and
 * identifiers which are of a more complex form than letters, numbers,
 * and underscores.
 */
public class BacktickQuotationCrystal extends AtonementCrystal {

    public BacktickQuotationCrystal() {
        super(TokenType.BACKTICK.getIdentifier());
    }
}
