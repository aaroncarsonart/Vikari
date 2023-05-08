package com.atonement.crystals.dnr.vikari.core.expression;

import com.atonement.crystals.dnr.vikari.core.crystal.UnaryOperatorCrystal;

public class UnaryExpression extends Expression {
    private UnaryOperatorCrystal operator;
    private Expression operand;

    public UnaryExpression(UnaryOperatorCrystal operator, Expression operand) {
        this.operator = operator;
        this.operand = operand;
        setLocation(operator.getCoordinates());
    }

    public UnaryOperatorCrystal getOperator() {
        return operator;
    }

    public void setOperator(UnaryOperatorCrystal operator) {
        this.operator = operator;
    }

    public Expression getOperand() {
        return operand;
    }

    public void setOperand(Expression operand) {
        this.operand = operand;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }
}
