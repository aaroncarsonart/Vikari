package com.atonement.crystals.dnr.vikari.core.expression;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;

public class LiteralExpression extends Expression {
    private AtonementCrystal value;

    public LiteralExpression(AtonementCrystal value) {
        this.value = value;
    }

    public AtonementCrystal getValue() {
        return value;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }
}
