package com.atonementcrystals.dnr.vikari.core.statement;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;

public class VariableDeclarationStatement extends Statement {
    private AtonementCrystal declaredVariable;

    // TODO: Refactor declarations to keep a declared type reference.
    //       The Interpreter will need to re-process all declarations
    //       because by the end of the TypeResolver's resolutions, all
    //       variables will contain their last assigned type.
    private TypeCrystal declaredType;

    private BinaryOperatorCrystal assignmentOperator;
    private Expression initializerExpression;

    public VariableDeclarationStatement(AtonementCrystal declaredVariable, BinaryOperatorCrystal assignmentOperator,
                                        Expression initializerExpression) {
        this.declaredVariable = declaredVariable;
        this.assignmentOperator = assignmentOperator;
        this.initializerExpression = initializerExpression;
    }

    public AtonementCrystal getDeclaredVariable() {
        return declaredVariable;
    }

    public BinaryOperatorCrystal getAssignmentOperator() {
        return assignmentOperator;
    }

    public Expression getInitializerExpression() {
        return initializerExpression;
    }

    @Override
    public <S> S accept(Visitor<S> visitor) {
        return visitor.visit(this);
    }
}
