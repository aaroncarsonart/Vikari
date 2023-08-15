package com.atonementcrystals.dnr.vikari.core.crystal.separator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The region operator :: has the following uses:
 *
 * <ol>
 *     <li>Denoting nested package references.</li>
 *     <li>Denoting nested region references.</li>
 *     <li>Beginning a new code block.</li>
 *     <li>
 *         Accessing a crystal or type's static field members
 *         from outside of its field definition statement.</li>
 *     <li>
 * </ol>
 */
public class RegionOperatorCrystal extends AtonementCrystal {

    public RegionOperatorCrystal() {
        super(TokenType.REGION_OPERATOR.getIdentifier());
    }

}