package com.atonement.crystals.dnr.vikari.core.separator.quotation;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * Use of two adjacent backticks `` denotes the beginning of a
 * capture quotation. Always used in pairs, these two identifiers
 * allow for arbitary quotation of text as a new String literal.
 */
public class CaptureQuotationCrystal extends AtonementCrystal {

    public CaptureQuotationCrystal() {
        super(TokenType.CAPTURE_QUOTATION.getIdentifier());
    }

}
