package com.atonementcrystals.dnr.vikari.core.expression;

/**
 * A NullLiteralExpression represents a statement of the form _[n]_. Where a sword crystal
 * is followed by a grouping expression which resolves to an integer, and then is followed
 * by another sword crystal.
 */
public class NullLiteralExpression extends Expression {
    private Expression expression;

    public NullLiteralExpression(Expression expression) {
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
