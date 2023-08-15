package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The line continuation operator ~ allows for individual statements to be
 * defined across multiple lines.<br/>
 * <br/>
 * The minimized form of this operator /~/ is used only for minimized code.
 * It is completely ignored by the interpreter. It is only parsed to support
 * syntax highlighting and to allow the regular line continuation operator
 * to be put back in place when the code is un-minimized.
 */
public class MinimizedLineContinuationCrystal extends AtonementCrystal {

    public MinimizedLineContinuationCrystal() {
        super(TokenType.MINIMIZED_LINE_CONTINUATION.getIdentifier());
    }

}
