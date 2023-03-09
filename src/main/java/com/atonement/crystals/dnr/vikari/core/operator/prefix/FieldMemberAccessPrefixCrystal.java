package com.atonement.crystals.dnr.vikari.core.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class FieldMemberAccessPrefixCrystal extends AtonementCrystal {

    public FieldMemberAccessPrefixCrystal() {
        super(DefaultIdentifierMapping.FIELD_MEMBER_ACCESS.getIdentifier());
    }
}
