package com.atonement.crystals.dnr.vikari.core.operator;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;

/**
 * The line continuation operator ~ allows for individual statements to be
 * defined across multiple lines.<br/>
 * <br/>
 * The minimized form of this operator /~/ is used only for minimized code.
 * It is completely ignored by the interpreter. It is only parsed to support
 * syntax highlighting and to allow the regular line continuation operator
 * to be put back in place when the code is un-minimized.
 */
public class LineContinuationMinimizedOperatorCrystal extends AtonementCrystal {

    public LineContinuationMinimizedOperatorCrystal() {
        super(TokenType.LINE_CONTINUATION_MINIMIZED.getIdentifier());
    }

}