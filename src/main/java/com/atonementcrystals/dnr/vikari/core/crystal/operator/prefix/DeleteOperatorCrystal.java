package com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The delete operator ~ deletes a crystal's field. The crystal's
 * identifier is then removed from its environment. This crystal
 * can then be redeclared with a new type. All identifiers that
 * share a reference to this field are unaffected. Use <code>
 * ~{crystal}</code> to delete all references to that crystal's
 * field instead.
 */
public class DeleteOperatorCrystal extends AtonementCrystal {

    public DeleteOperatorCrystal() {
        super(TokenType.DELETE.getIdentifier());
    }

}
