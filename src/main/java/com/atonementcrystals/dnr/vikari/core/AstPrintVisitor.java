package com.atonementcrystals.dnr.vikari.core;

import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.PrintExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.statement.BlankStatement;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.PrintStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.SyntaxErrorStatement;
import com.atonementcrystals.dnr.vikari.util.Utils;

/**
 * Walk the AST and print a string representation of each statement and expression.
 */
public class AstPrintVisitor implements Statement.Visitor<String>, Expression.Visitor<String> {
    boolean verbose;

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    private String group(String... args) {
        StringBuilder sb = new StringBuilder();

        if (verbose) {
            String label = args[0];
            sb.append(label);
            sb.append(":");
        }
        sb.append('[');

        for (int i = 1; i < args.length; i++) {
            String arg = args[i];
            sb.append(arg);
            if (i < args.length - 1) {
                sb.append(" ");
            }
        }
        sb.append(']');
        return sb.toString();
    }

    @Override
    public String visit(BinaryExpression expr) {
        String left = expr.getLeft().accept(this);
        String operator = expr.getOperator().getIdentifier();
        String right = expr.getRight().accept(this);

        String descriptor = Utils.getSimpleClassName(expr.getOperator());
        return group(descriptor, left, operator, right);
    }

    @Override
    public String visit(GroupingExpression expr) {
        return group("Grouping", expr.getExpression().accept(this));
    }

    @Override
    public String visit(LiteralExpression expr) {
        return expr.getValue().getIdentifier();
    }

    @Override
    public String visit(PrintExpression expr) {
        String operator = expr.getPrintOperatorCrystal().getIdentifier();
        Expression operand = expr.getExpression();
        if (operand != null) {
            return operator + operand.accept(this);
        }
        return operator;
    }

    @Override
    public String visit(UnaryExpression expr) {
        String operator = expr.getOperator().getIdentifier();
        String operand = expr.getOperand().accept(this);
        String descriptor = Utils.getSimpleClassName(expr.getOperator());
        return group(descriptor, operator, operand);
    }

    @Override
    public String visit(PrintStatement stmt) {
        StringBuilder sb = new StringBuilder();
        for (PrintExpression printExpression : stmt.getPrintExpressions()) {
            sb.append(printExpression.accept(this));
        }
        return sb.toString();
    }

    @Override
    public String visit(ExpressionStatement stmt) {
        return stmt.getExpression().accept(this);
    }

    @Override
    public String visit(SyntaxErrorStatement stmt) {
        String statement = stmt.getStatement();
        return "Syntax Error: [" + statement + "]";
    }

    @Override
    public String visit(BlankStatement stmt) {
        // Will never be visited. So throw an internal error!
        throw new IllegalStateException("Unreachable code.");
    }
}
