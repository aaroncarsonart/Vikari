package com.atonement.crystals.dnr.vikari.core.expression;

public class GroupingExpression extends Expression {
    private Expression expression;

    public GroupingExpression(Expression expression) {
        this.expression = expression;
    }

    public Expression getExpression() {
        return expression;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }

}
