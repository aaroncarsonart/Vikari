package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

public abstract class UnaryOperatorCrystal extends AtonementCrystal {

    public UnaryOperatorCrystal(String identifier) {
        super(identifier);
    }

    public abstract AtonementCrystal evaluate(AtonementCrystal operand);

}
