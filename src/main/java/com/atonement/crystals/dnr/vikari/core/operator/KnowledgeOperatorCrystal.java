package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The knowledge operator : either shares knowledge of the running
 * Vikari program's state by printing it to standard output, or by
 * explicitly either defining or typecasting an identifer's Type.
 */
public class KnowledgeOperatorCrystal extends AtonementCrystal {

    public KnowledgeOperatorCrystal() {
        super(DefaultIdentifierMapping.KNOWLEDGE_OPERATOR.getIdentifier());
    }
}

