package com.atonementcrystals.dnr.vikari.core.expression;

import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;

@FunctionalInterface
public interface BinaryExpressionConstructor {
    Expression construct(Expression left, BinaryOperatorCrystal operator, Expression right);
}
