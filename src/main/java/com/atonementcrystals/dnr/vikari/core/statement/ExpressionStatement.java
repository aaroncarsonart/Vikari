package com.atonementcrystals.dnr.vikari.core.statement;

import com.atonementcrystals.dnr.vikari.core.expression.Expression;

public class ExpressionStatement extends Statement {
    private Expression expression;

    public ExpressionStatement(Expression expression) {
        this.expression = expression;
        setLocation(expression.getLocation());
    }

    public Expression getExpression() {
        return expression;
    }

    public void setExpression(Expression expression) {
        this.expression = expression;
    }

    @Override
    public <S> S accept(Visitor<S> visitor) {
        return visitor.visit(this);
    }
}
