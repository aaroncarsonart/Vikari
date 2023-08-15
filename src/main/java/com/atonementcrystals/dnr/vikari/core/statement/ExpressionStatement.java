package com.atonementcrystals.dnr.vikari.core.statement;

import com.atonementcrystals.dnr.vikari.core.expression.Expression;

public class ExpressionStatement extends Statement {
    private final Expression expression;

    public ExpressionStatement(Expression expression) {
        this.expression = expression;
        setLocation(expression.getLocation());
    }

    public Expression getExpression() {
        return expression;
    }

    @Override
    public <S> S accept(Visitor<S> visitor) {
        return visitor.visit(this);
    }
}
