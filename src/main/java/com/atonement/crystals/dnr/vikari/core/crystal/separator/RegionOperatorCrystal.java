package com.atonement.crystals.dnr.vikari.core.crystal.separator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The region operator :: has the following uses:
 *
 * <ol>
 *     <li>Denoting nested package references.</li>
 *     <li>Denoting nested region references.</li>
 *     <li>Beginning a new code block.</li>
 *     <li>
 *         Accessing a type's static members
 *         outside of its field definition.</li>
 *     <li>
 * </ol>
 */
public class RegionOperatorCrystal extends AtonementCrystal {

    public RegionOperatorCrystal() {
        super(TokenType.REGION_OPERATOR.getIdentifier());
    }

}