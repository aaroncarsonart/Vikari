package com.atonement.crystals.dnr.vikari.core.operator.prefix;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The copy constructor & unary operator makes a shallow copy
 * of a crystal by copying all of its type members into a new
 * crystal instance, and then returning that instance.<br/>
 * <br/>
 * Default behavior of & can be changed by overriding the
 * <code>copy!()</code> dnr method.
 */
public class CopyConstructorCrystal extends AtonementCrystal {

    public CopyConstructorCrystal() {
        super(TokenType.COPY_CONSTRUCTOR.getIdentifier());
    }

}
