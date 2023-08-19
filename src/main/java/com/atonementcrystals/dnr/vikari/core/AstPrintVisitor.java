package com.atonementcrystals.dnr.vikari.core;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.BooleanLogicExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.NullLiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.PrintExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.PrintStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.SyntaxErrorStatement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.util.Utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Walk the AST and print a string representation of each statement and expression.
 */
public class AstPrintVisitor implements Statement.Visitor<String>, Expression.Visitor<String> {

    /**
     * A shared AstPrintVisitor instance for debugging purposes.
     */
    public static final AstPrintVisitor INSTANCE = new AstPrintVisitor();

    /**
     * If true, then an additional string label describing the Expression's type
     * is prepended before the additional [ ] grouping of which wraps most sub-
     * Expressions in a Statement.
     */
    private boolean verbose;

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    private String group(String descriptor, String... args) {
        StringBuilder sb = new StringBuilder();

        if (verbose) {
            sb.append(descriptor);
            sb.append(":");
        }

        if (verbose || args.length > 1) {
            sb.append('[');
        }

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            sb.append(arg);
            if (i < args.length - 1) {
                sb.append(", ");
            }
        }

        if (verbose || args.length > 1) {
            sb.append(']');
        }

        return sb.toString();
    }

    private String visit(AtonementCrystal operator) {
        String descriptor = Utils.getSimpleClassName(operator);
        return group(descriptor, operator.getIdentifier());
    }

    @Override
    public String visit(BinaryExpression expr) {
        String left = expr.getLeft().accept(this);
        String operator = visit(expr.getOperator());
        String right = expr.getRight().accept(this);

        String descriptor = "BinaryExpr";
        return group(descriptor, left, operator, right);
    }

    @Override
    public String visit(BooleanLogicExpression expr) {
        String left = expr.getLeft().accept(this);
        String operator = visit(expr.getOperator());
        String right = expr.getRight().accept(this);

        String descriptor = "LogicalExpr";
        return group(descriptor, left, operator, right);
    }

    @Override
    public String visit(GroupingExpression expr) {
        return group("GroupingExpr", expr.getExpression().accept(this));
    }

    @Override
    public String visit(LiteralExpression expr) {
        String descriptor = "LiteralExpr";
        return group(descriptor, expr.getValue().getIdentifier());
    }

    @Override
    public String visit(PrintExpression expr) {
        String operator = visit(expr.getPrintOperatorCrystal());
        Expression operand = expr.getExpression();

        String descriptor = "PrintExpr";
        if (operand != null) {
            return group(descriptor, operator, operand.accept(this));
        }
        return group(descriptor, operator);
    }

    @Override
    public String visit(UnaryExpression expr) {
        String operator = visit(expr.getOperator());
        String operand = expr.getOperand().accept(this);
        String descriptor = "UnaryExpr";
        return group(descriptor, operator, operand);
    }

    @Override
    public String visit(VariableExpression expr) {
        AtonementCrystal reference = expr.getReference();
        String identifier = reference.getIdentifier();
        String descriptor = "VariableExpr";
        return group(descriptor, identifier);
    }

    @Override
    public String visit(VariableDeclarationStatement stmt) {
        String descriptor = "VariableDeclarationStmt";

        AtonementCrystal declaredVariable = stmt.getDeclaredVariable();
        String declaredVariableString = declaredVariable.getIdentifier();
        TypeCrystal declaredType = declaredVariable.getDeclaredType();

        if (declaredType != null) {
            declaredVariableString += ":" + declaredType.getIdentifier();
        }

        if (stmt.getInitializerExpression() != null) {
            String operator = visit(stmt.getAssignmentOperator());
            String initializerExpression = stmt.getInitializerExpression().accept(this);
            return group(descriptor, declaredVariableString, operator, initializerExpression);
        }

        return group(descriptor, declaredVariableString);
    }

    @Override
    public String visit(LeftAssignmentExpression expr) {
        String lvalue = expr.getLvalue().accept(this);
        String operator = visit(expr.getOperator());
        String rvalue = expr.getRvalue().accept(this);
        String descriptor = "LeftAssignmentExpr";
        return group(descriptor, lvalue, operator, rvalue);
    }

    @Override
    public String visit(RightAssignmentExpression expr) {
        String rvalue = expr.getRvalue().accept(this);
        String operator = visit(expr.getOperator());
        String lvalue = expr.getLvalue().accept(this);
        String descriptor = "RightAssignmentExpr";
        return group(descriptor, rvalue, operator, lvalue);
    }

    @Override
    public String visit(NullLiteralExpression expr) {
        String expressionResult = expr.getExpression().accept(this);
        String descriptor = "NullLiteralExpr";
        return group(descriptor, expressionResult);
    }

    @Override
    public String visit(PrintStatement stmt) {
        List<String> groupArgs = new ArrayList<>();
        for (PrintExpression printExpression : stmt.getPrintExpressions()) {
            groupArgs.add(printExpression.accept(this));
        }
        String descriptor = "PrintStmt";
        return group(descriptor, groupArgs.toArray(new String[0]));
    }

    @Override
    public String visit(ExpressionStatement stmt) {
        String descriptor = "ExpressionStmt";
        return group(descriptor, stmt.getExpression().accept(this));
    }

    @Override
    public String visit(SyntaxErrorStatement stmt) {
        String statement = stmt.getStatement();
        String descriptor = "SyntaxErrorStmt";
        return group(descriptor, statement);
    }
}
