package com.atonementcrystals.dnr.vikari.core.statement;

import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

public class BlankStatement extends Statement {
    public BlankStatement(CoordinatePair location) {
        this.setLocation(location);
    }

    @Override
    public <S> S accept(Visitor<S> visitor) {
        return visitor.visit(this);
    }
}
