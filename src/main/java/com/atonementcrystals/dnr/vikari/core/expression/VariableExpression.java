package com.atonementcrystals.dnr.vikari.core.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

public class VariableExpression extends Expression {
    AtonementCrystal reference;

    public VariableExpression(AtonementCrystal reference) {
        this.reference = reference;
        this.setLocation(reference.getCoordinates());
    }

    public AtonementCrystal getReference() {
        return reference;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }
}
