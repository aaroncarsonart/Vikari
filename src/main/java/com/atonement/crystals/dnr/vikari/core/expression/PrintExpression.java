package com.atonement.crystals.dnr.vikari.core.expression;

import com.atonement.crystals.dnr.vikari.core.crystal.operator.PrintStatementOperatorCrystal;

public class PrintExpression extends Expression {
    private PrintStatementOperatorCrystal printOperatorCrystal;
    private Expression expression;

    public PrintExpression(PrintStatementOperatorCrystal printOperatorCrystal, Expression expression) {
        this.printOperatorCrystal = printOperatorCrystal;
        this.expression = expression;
        setLocation(printOperatorCrystal.getCoordinates());
    }

    public PrintStatementOperatorCrystal getPrintOperatorCrystal() {
        return printOperatorCrystal;
    }

    public Expression getExpression() {
        return expression;
    }

    @Override
    public <E> E accept(Visitor<E> visitor) {
        return visitor.visit(this);
    }
}
