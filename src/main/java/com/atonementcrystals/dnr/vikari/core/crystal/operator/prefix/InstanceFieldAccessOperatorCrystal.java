package com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The unary instance field access operator @ is used in two cases:
 * <ol>
 *     <li>Declare an instance type member.</li>
 *     <li>Access an instance type member.</li>
 * </ol>
 * @ cannot be used outside a type's field declaration statement.
 */
public class InstanceFieldAccessOperatorCrystal extends AtonementCrystal {

    public InstanceFieldAccessOperatorCrystal() {
        super(TokenType.INSTANCE_FIELD_ACCESS.getIdentifier());
    }

}
