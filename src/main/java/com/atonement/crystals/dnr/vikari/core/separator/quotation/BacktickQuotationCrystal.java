package com.atonement.crystals.dnr.vikari.core.separator.quotation;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

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
