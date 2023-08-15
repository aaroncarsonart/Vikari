package com.atonementcrystals.dnr.vikari.core.crystal.operator.angelguard;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The left \\ and right // feather-fall operators are used in pairs to silently
 * catch all errors thrown by the series of statements included between them.<br/>
 * <br/>
 * <pre>  \\ dangerousFunction!() //</pre>
 * <br/>
 * This is equivalent to writing:<br/>
 * <br/><pre>
 * __ __ ::
 *   dangerousFunction!()
 * || :: __</pre>
 */
public class LeftFeatherFallCrystal extends AtonementCrystal {

    public LeftFeatherFallCrystal() {
        super(TokenType.LEFT_FEATHER_FALL.getIdentifier());
    }

}
