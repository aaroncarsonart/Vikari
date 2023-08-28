package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The type label operator : denotes static typing. It is used in variable
 * declaration statements, with the exists operator ?, and in function
 * parameter lists and return types.<br/>
 * <br/>
 * For expressions involving typing of a variable, the left operand is a
 * variable name, and the right operand is a type name:<br/>
 * <pre>
 * variable:Integer</pre>
 *
 * For function return types, it is located at the end of the parameter
 * list but before the region operator:<br/>
 * <pre>
 * function << (x:Int|y:Int): Int :: x + y</pre>
 */
public class TypeLabelOperatorCrystal extends AtonementCrystal {

    public TypeLabelOperatorCrystal() {
        super(TokenType.TYPE_LABEL.getIdentifier());
    }

}

