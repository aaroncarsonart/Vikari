package com.atonement.crystals.dnr.vikari.core.separator.quotation;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * Use of a singular backtick ` denotes the beginning of quoting
 * any Vikari identifier. They are always used in pairs, just as
 * a singular quote ' is used in other languages.
 */
public class BacktickCrystal extends AtonementCrystal {
    public BacktickCrystal() {
        super(DefaultIdentifierMapping.BACKTICK.getIdentifier());
    }
}
