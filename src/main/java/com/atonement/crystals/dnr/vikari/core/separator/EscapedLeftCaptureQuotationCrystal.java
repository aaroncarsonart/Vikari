package com.atonement.crystals.dnr.vikari.core.separator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class EscapedLeftCaptureQuotationCrystal extends AtonementCrystal {
    public EscapedLeftCaptureQuotationCrystal() {
        super(DefaultIdentifierMapping.ESCAPED_RIGHT_CAPTURE_QUOTATION.getIdentifier());
    }
}