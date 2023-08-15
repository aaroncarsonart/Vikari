package com.atonementcrystals.dnr.vikari.core.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

/**
 * Left assignment expressions contain an lvalue, which is the target
 * being assigned to, and an rvalue, which is the value being assigned.
 */
public class LeftAssignmentExpression extends Expression {
    private Expression lvalue;
    private BinaryOperatorCrystal operator;
    private Expression rvalue;

    public LeftAssignmentExpression(Expression lvalue, BinaryOperatorCrystal operator, Expression rvalue) {
        this.lvalue = lvalue;
        this.operator = operator;
        this.rvalue = rvalue;
        this.setLocation(lvalue.getLocation());
    }

    public Expression getLvalue() {
        return lvalue;
    }

    public BinaryOperatorCrystal getOperator() {
        return operator;
    }

    public Expression getRvalue() {
        return rvalue;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }
}
