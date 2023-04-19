package com.atonement.crystals.dnr.vikari.core.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The unary instance field access operator @ is used in two cases:
 * <ol>
 *     <li>Declare an instance type member.</li>
 *     <li>Access an instance type member.</li>
 * </ol>
 * @ cannot be used outside of a type's field declaration.
 */
public class InstanceFieldAccessOperatorCrystal extends AtonementCrystal {

    public InstanceFieldAccessOperatorCrystal() {
        super(TokenType.INSTANCE_FIELD_ACCESS.getIdentifier());
    }

}
