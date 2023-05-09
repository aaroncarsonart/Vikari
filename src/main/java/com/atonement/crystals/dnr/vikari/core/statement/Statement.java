package com.atonement.crystals.dnr.vikari.core.statement;

import com.atonement.crystals.dnr.vikari.util.CoordinatePair;

public abstract class Statement {
    private CoordinatePair location;

    public CoordinatePair getLocation() {
        return location;
    }

    public void setLocation(CoordinatePair location) {
        this.location = location;
    }

    public interface Visitor<S> {
        S visit(PrintStatement stmt);
        S visit(ExpressionStatement stmt);
        S visit(SyntaxErrorStatement stmt);
    }

    public abstract <S> S accept(Visitor<S> visitor);
}
