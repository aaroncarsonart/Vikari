package com.atonementcrystals.dnr.vikari.core.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

/**
 * Right assignment expressions switch the order of operands. But the lvalue is
 * still the value being assigned to, and the rvalue is the value being assigned.
 */
public class RightAssignmentExpression extends Expression {
    private Expression rvalue;
    private BinaryOperatorCrystal operator;
    private Expression lvalue;

    public RightAssignmentExpression(Expression rvalue, BinaryOperatorCrystal operator, Expression lvalue) {
        this.rvalue = rvalue;
        this.operator = operator;
        this.lvalue = lvalue;
        this.setLocation(rvalue.getLocation());
    }

    public Expression getRvalue() {
        return rvalue;
    }

    public BinaryOperatorCrystal getOperator() {
        return operator;
    }

    public Expression getLvalue() {
        return lvalue;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }
}
