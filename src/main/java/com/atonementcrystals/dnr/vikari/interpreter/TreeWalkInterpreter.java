package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.PrintExpression;
import com.atonementcrystals.dnr.vikari.core.statement.BlankStatement;
import com.atonementcrystals.dnr.vikari.core.statement.PrintStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.SyntaxErrorStatement;
import com.atonementcrystals.dnr.vikari.error.RuntimeError;
import com.atonementcrystals.dnr.vikari.error.Vikari_RuntimeException;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.UnaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.LeftDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.NegateCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.RightDivideOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.List;
import java.util.stream.Collectors;

public class TreeWalkInterpreter implements Statement.Visitor<AtonementCrystal>, Expression.Visitor<AtonementCrystal> {
    private static final Logger log = LogManager.getLogger(TreeWalkInterpreter.class);

    private File currentFile;
    private List<List<AtonementCrystal>> lexedStatements;

    public void setLexedStatements(List<List<AtonementCrystal>> lexedStatements) {
        this.lexedStatements = lexedStatements;
    }

    public void interpret(File file, List<Statement> statements) {
        this.currentFile = file;
        try {
            for (Statement statement : statements) {
                execute(statement);
            }
        } catch (Vikari_RuntimeException e) {
            reportError(e);
        }
        this.currentFile = null;
    }

    private void reportError(Vikari_RuntimeException e) {
        RuntimeError runtimeError = e.getRuntimeError();
        String errorReport = runtimeError.getErrorReport();
        System.out.println(errorReport);
        log.debug("\n{}", errorReport);
    }

    public AtonementCrystal execute(Statement statement) {
        return statement.accept(this);
    }

    public AtonementCrystal evaluate(Expression expression) {
        return expression.accept(this);
    }

    @Override
    public AtonementCrystal visit(BinaryExpression expr) {
        AtonementCrystal left = evaluate(expr.getLeft());
        AtonementCrystal right = evaluate(expr.getRight());
        BinaryOperatorCrystal operator = expr.getOperator();

        AtonementCrystal result = null;
        if (operator instanceof AddOperatorCrystal) {
            if (left instanceof NumberCrystal && right instanceof NumberCrystal) {
                result = Arithmetic.add((NumberCrystal) left, (NumberCrystal) right);
            }
        } else if (operator instanceof SubtractOperatorCrystal) {
            if (left instanceof NumberCrystal && right instanceof NumberCrystal) {
                result = Arithmetic.subtract((NumberCrystal) left, (NumberCrystal) right);
            }
        } else if (operator instanceof MultiplyOperatorCrystal) {
            if (left instanceof NumberCrystal && right instanceof NumberCrystal) {
                result = Arithmetic.multiply((NumberCrystal) left, (NumberCrystal) right);
            }
        } else if (operator instanceof LeftDivideOperatorCrystal) {
            if (left instanceof NumberCrystal && right instanceof NumberCrystal) {
                result = Arithmetic.divide((NumberCrystal) left, (NumberCrystal) right);
            }
        } else if (operator instanceof RightDivideOperatorCrystal) {
            if (left instanceof NumberCrystal && right instanceof NumberCrystal) {
                result = Arithmetic.divide((NumberCrystal) right, (NumberCrystal) left);
            }
        } else {
            throw internalRuntimeErrorForUnexpectedOperator(operator);
        }

        if (result == null) {
            throw internalRuntimeErrorForUnexpectedBinaryOperands(operator);
        }

        return result;
    }

    @Override
    public AtonementCrystal visit(GroupingExpression expr) {
        return evaluate(expr.getExpression());
    }

    @Override
    public AtonementCrystal visit(LiteralExpression expr) {
        return expr.getValue();
    }

    @Override
    public AtonementCrystal visit(PrintExpression expr) {
        // TODO: Should return a StringCrystal.
        // TODO: (Once StringCrystals are implmemented in Vikari.)
        Expression innerExpression = expr.getExpression();
        if (innerExpression == null) {
            System.out.println();
            return null;
        } else {
            AtonementCrystal result = evaluate(expr.getExpression());
            String output = result.getStringRepresentation();
            System.out.print(output);
            return result;
        }
    }

    @Override
    public AtonementCrystal visit(UnaryExpression expr) {
        AtonementCrystal operand = evaluate(expr.getOperand());
        UnaryOperatorCrystal operator = expr.getOperator();
        AtonementCrystal result = null;

        if (operator instanceof NegateCrystal) {
            if (operand instanceof NumberCrystal) {
                result = Arithmetic.negate((NumberCrystal) operand);
            }
        } else {
            throw internalRuntimeErrorForUnexpectedOperator(operator);
        }

        if (result == null) {
            throw internalRuntimeErrorForUnexpectedUnaryOperand(operator);
        }

        return result;
    }

    @Override
    public AtonementCrystal visit(PrintStatement stmt) {
        for (PrintExpression printExpression : stmt.getPrintExpressions()) {
            evaluate(printExpression);
        }
        // TODO: Return a String for what is printed.
        return null;
    }

    @Override
    public AtonementCrystal visit(ExpressionStatement stmt) {
        return evaluate(stmt.getExpression());
    }

    @Override
    public AtonementCrystal visit(SyntaxErrorStatement stmt) {
        // This is always an internal error. And so it never should occur!
        CoordinatePair location = stmt.getLocation();
        String errorMessage = "Statement containing a Syntax Error should not be evaluated.";
        throw internalRuntimeError(location, errorMessage);
    }

    @Override
    public AtonementCrystal visit(BlankStatement stmt) {
        // This is always an internal error. And so it never should occur!
        CoordinatePair location = stmt.getLocation();
        String errorMessage = "Blank statements should not be evaluated.";
        throw internalRuntimeError(location, errorMessage);
    }

    private Vikari_RuntimeException internalRuntimeError(CoordinatePair location, String errorMessage) {
        int lineNumber = location.getRow();
        List<AtonementCrystal> lexedLine = lexedStatements.get(lineNumber);
        String line = lexedLine.stream().map(AtonementCrystal::getIdentifier).collect(Collectors.joining());
        RuntimeError runtimeError = new RuntimeError(currentFile, location, line, errorMessage);
        return new Vikari_RuntimeException("Internal Error", runtimeError);
    }

    private Vikari_RuntimeException internalRuntimeErrorForUnexpectedOperator(AtonementCrystal operator) {
        CoordinatePair location = operator.getCoordinates();
        String errorMessage = "Unexpected operator: ``" + operator.getIdentifier() + "``.";
        return internalRuntimeError(location, errorMessage);
    }

    private Vikari_RuntimeException internalRuntimeErrorForUnexpectedUnaryOperand(AtonementCrystal operator) {
        CoordinatePair location = operator.getCoordinates();
        String errorMessage = "Invalid operand for operator ``" + operator.getIdentifier() + "``.";
        return internalRuntimeError(location, errorMessage);
    }

    private Vikari_RuntimeException internalRuntimeErrorForUnexpectedBinaryOperands(AtonementCrystal operator) {
        CoordinatePair location = operator.getCoordinates();
        String errorMessage = "Invalid operands for operator ``" + operator.getIdentifier() + "``.";
        return internalRuntimeError(location, errorMessage);
    }

    public void clear() {
        currentFile = null;
        lexedStatements = null;
    }
}
