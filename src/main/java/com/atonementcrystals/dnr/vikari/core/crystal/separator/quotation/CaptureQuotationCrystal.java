package com.atonementcrystals.dnr.vikari.core.crystal.separator.quotation;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * Use of two adjacent backticks `` denotes the beginning of a
 * capture quotation. Always used in pairs, these two identifiers
 * allow for arbitrary quotation of text as a new String literal.
 */
public class CaptureQuotationCrystal extends AtonementCrystal {

    public CaptureQuotationCrystal() {
        super(TokenType.CAPTURE_QUOTATION.getIdentifier());
    }

}
