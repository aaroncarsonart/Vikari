package com.atonement.crystals.dnr.vikari.core.crystal.operator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The constructor operator * instantiates a type instance.
 * This can be used to create a new crystal instance without
 * specifying <code>TypeName!</code> in an assignment statement.<br/>
 * <br/>
 * A type's constructors are defined by assigning a function to *.<br/>
 * <br/>
 * * can be used to instantiate a new base AtonementCrystal when
 * no type label is provided for a variable; or, when passing
 * arguments to a function or list literal.
 */
public class ConstructorCrystal extends AtonementCrystal {

    public ConstructorCrystal() {
        super(TokenType.CONSTRUCTOR.getIdentifier());
    }

}
