package com.atonement.crystals.dnr.vikari.core.crystal.operator.angelguard;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The left \\ and right // feather-fall operators are used in pairs to silently
 * catch all errors thrown by the series of statements included between them.<br/>
 * <br/>
 * <code>    \\ dangerousFunction!() //</code><br/>
 * <br/>
 * This is equivalent to writing:<br/>
 * <br/>
 * <code>__ __ :: dangerousFunction!() <br/>
 * || :: _</code>
 */
public class RightFeatherFallCrystal extends AtonementCrystal {

    public RightFeatherFallCrystal() {
        super(TokenType.RIGHT_FEATHER_FALL.getIdentifier());
    }

}
