package com.atonement.crystals.dnr.vikari.core.separator.quotation;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * Use of two adjacent backticks `` denotes the beginning of a
 * capture quotation. Always used in pairs, these two identifiers
 * allow for arbitary quotation of text as a new String literal.
 * However, they also can denote arbitrary code blocks of a nested
 * scope. Such as when used with crystal and function definitions,
 * as well as conditional branch and loop statements.
 */
public class CaptureQuotationCrystal extends AtonementCrystal {
    public CaptureQuotationCrystal() {
        super(DefaultIdentifierMapping.CAPTURE_QUOTATION.getIdentifier());
    }
}
