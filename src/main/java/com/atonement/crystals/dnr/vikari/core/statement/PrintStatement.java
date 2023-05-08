package com.atonement.crystals.dnr.vikari.core.statement;

import com.atonement.crystals.dnr.vikari.core.expression.PrintExpression;

import java.util.List;

public class PrintStatement extends Statement {
    private List<PrintExpression> printExpressions;

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
