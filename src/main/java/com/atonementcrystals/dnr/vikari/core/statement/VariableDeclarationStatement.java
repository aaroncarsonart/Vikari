package com.atonementcrystals.dnr.vikari.core.statement;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;

public class VariableDeclarationStatement extends Statement {
    private final AtonementCrystal declaredVariable;
    private final TypeCrystal declaredType;
    private final BinaryOperatorCrystal assignmentOperator;
    private final Expression initializerExpression;

    /** For resetting after a syntax error in VikariREPL. */
    private AtonementField environment;

    public VariableDeclarationStatement(AtonementCrystal declaredVariable, TypeCrystal declaredType,
                                        BinaryOperatorCrystal assignmentOperator, Expression initializerExpression) {
        this.declaredVariable = declaredVariable;
        this.declaredType = declaredType;
        this.assignmentOperator = assignmentOperator;
        this.initializerExpression = initializerExpression;
    }

    public AtonementCrystal getDeclaredVariable() {
        return declaredVariable;
    }

    public TypeCrystal getDeclaredType() {
        return declaredType;
    }

    public BinaryOperatorCrystal getAssignmentOperator() {
        return assignmentOperator;
    }

    public Expression getInitializerExpression() {
        return initializerExpression;
    }

    public AtonementField getEnvironment() {
        return environment;
    }

    public void setEnvironment(AtonementField environment) {
        this.environment = environment;
    }

    @Override
    public <S> S accept(Visitor<S> visitor) {
        return visitor.visit(this);
    }
}
