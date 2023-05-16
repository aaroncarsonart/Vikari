package com.atonementcrystals.dnr.vikari.core.expression;

import com.atonementcrystals.dnr.vikari.core.AstPrintVisitor;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

public abstract class Expression {
    private CoordinatePair location;

    public CoordinatePair getLocation() {
        return location;
    }

    public void setLocation(CoordinatePair location) {
        this.location = location;
    }

    public interface Visitor<E> {
        E visit(BinaryExpression expr);
        E visit(GroupingExpression expr);
        E visit(LiteralExpression expr);
        E visit(PrintExpression expr);
        E visit(UnaryExpression expr);
    }

    public abstract <E> E accept(Visitor<E> visitor);

    /**
     * @return An AstPrintVisitor String representation for debugging purposes.
     */
    @Override
    public String toString() {
        return this.accept(AstPrintVisitor.INSTANCE);
    }
}
