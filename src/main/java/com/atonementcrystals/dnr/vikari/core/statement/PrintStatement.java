package com.atonementcrystals.dnr.vikari.core.statement;

import com.atonementcrystals.dnr.vikari.core.expression.PrintExpression;

import java.util.List;

public class PrintStatement extends Statement {
    private final List<PrintExpression> printExpressions;

    public PrintStatement(List<PrintExpression> printExpressions) {
        this.printExpressions = printExpressions;
    }

    public List<PrintExpression> getPrintExpressions() {
        return printExpressions;
    }

    @Override
    public <S> S accept(Visitor<S> visitor) {
        return visitor.visit(this);
    }
}
