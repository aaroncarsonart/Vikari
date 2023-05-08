package com.atonement.crystals.dnr.vikari.core.expression;

import com.atonement.crystals.dnr.vikari.util.CoordinatePair;

public abstract class Expression {
    private CoordinatePair location;

    public CoordinatePair getLocation() {
        return location;
    }

    public void setLocation(CoordinatePair location) {
        this.location = location;
    }

    public interface Visitor<E> {
        E visit(BinaryExpression expr);
        E visit(GroupingExpression expr);
        E visit(LiteralExpression expr);
        E visit(PrintExpression expr);
        E visit(UnaryExpression expr);
    }

    public abstract <E> E accept(Visitor<E> visitor);
}
