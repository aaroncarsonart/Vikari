package com.atonement.crystals.dnr.vikari.core.keyword.label;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The `import` access modifier keyword in conjunction with `::` produces
 * a label after which all fully-qualified names of Types are declared in
 * order to become accessible to the crystal's field definition.
 */
public class ImportCrystal extends AtonementCrystal {
    public ImportCrystal() {
        super(DefaultIdentifierMapping.IMPORT.getIdentifier());
    }
}
