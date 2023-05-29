package com.atonementcrystals.dnr.vikari.core.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

public class BinaryExpression extends Expression {
    private Expression left;
    private BinaryOperatorCrystal operator;
    private Expression right;

    public BinaryExpression(Expression left, BinaryOperatorCrystal operator, Expression right) {
        this.left = left;
        this.operator = operator;
        this.right = right;
        setLocation(left.getLocation());
    }

    public Expression getLeft() {
        return left;
    }

    public BinaryOperatorCrystal getOperator() {
        return operator;
    }

    public Expression getRight() {
        return right;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }
}
