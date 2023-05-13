package com.atonementcrystals.dnr.vikari.core.statement;

public class SyntaxErrorStatement extends Statement {

    String statement;

    public SyntaxErrorStatement(String statement) {
        this.statement = statement;
    }

    public String getStatement() {
        return statement;
    }

    @Override
    public <S> S accept(Visitor<S> visitor) {
        return visitor.visit(this);
    }
}
