package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeHierarchy;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullKeywordCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalAndOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.logical.LogicalOrOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.BinaryExpression;
import com.atonementcrystals.dnr.vikari.core.expression.BooleanLogicExpression;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.NullLiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.PrintExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.VariableExpression;
import com.atonementcrystals.dnr.vikari.core.statement.PrintStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.SyntaxErrorStatement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.RuntimeError;
import com.atonementcrystals.dnr.vikari.error.Vikari_RuntimeException;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.UnaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.GroupingExpression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.expression.UnaryExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

/**
 * Interpret the output of the Parser using a simple tree-walking approach.
 */
public class TreeWalkInterpreter implements Statement.Visitor<AtonementCrystal>, Expression.Visitor<AtonementCrystal> {
    private static final Logger log = LogManager.getLogger(TreeWalkInterpreter.class);

    private File currentFile;
    private BiFunction<File, Integer, String> getLineFunction;

    /** Parent field of the rootEnvironment. */
    private AtonementField globalAtonementField;

    /** Root environments are unique per source file. */
    private Map<String, AtonementField> rootEnvironments;
    private AtonementField rootEnvironment;
    private AtonementField currentEnvironment;

    public TreeWalkInterpreter() {
        rootEnvironments = new HashMap<>();
    }

    /**
     * Set the function to get the line data for the current file in order to properly
     * report RuntimeErrors in a helpful way to the end user.
     * @param getLineFunction The function to fetch the line data with. It accepts a File
     *                        and a line number, and returns a String.
     */
    public void setGetLineFunction(BiFunction<File, Integer, String> getLineFunction) {
        this.getLineFunction = getLineFunction;
    }

    public void interpret(File file, List<Statement> statements) {
        log.trace("interpret({})", file == null ? "null" : "\"" + file + "\"");
        this.currentFile = file;
        establishRootEnvironment();
        try {
            for (Statement statement : statements) {
                execute(statement);
            }
        } catch (Vikari_RuntimeException e) {
            reportError(e);
        }
        this.currentFile = null;
    }

    public void setGlobalAtonementField(AtonementField globalAtonementField) {
        this.globalAtonementField = globalAtonementField;
    }

    public void establishRootEnvironment() {
        // File is for a type or a script.
        // Cache the environment with the file path.
        if (currentFile != null) {
            String filePath = currentFile.getAbsolutePath();
            if (rootEnvironments.containsKey(filePath)) {
                rootEnvironment = rootEnvironments.get(filePath);
            } else {
                rootEnvironment = new AtonementField(globalAtonementField);
                rootEnvironments.put(filePath, rootEnvironment);
            }
        }

        // Code is executed with -c or the REPL.
        // So only one environment is necessary.
        else if (rootEnvironment == null) {
            rootEnvironment = new AtonementField(globalAtonementField);
        }
        currentEnvironment = rootEnvironment;
    }

    public AtonementField getRootEnvironment() {
        return rootEnvironment;
    }

    public AtonementField getCurrentEnvironment() {
        return currentEnvironment;
    }

    public void reportError(Vikari_RuntimeException e) {
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

        AtonementCrystal result;

        try {
            result = operator.evaluate(left, right);
        } catch (UnsupportedOperationException e) {
            throw internalRuntimeErrorForUnexpectedOperator(operator);
        }

        if (result == null) {
            throw internalRuntimeErrorForUnexpectedBinaryOperands(operator);
        }

        return result;
    }

    @Override
    public AtonementCrystal visit(BooleanLogicExpression expr) {
        AtonementCrystal left = evaluate(expr.getLeft());
        BinaryOperatorCrystal operator = expr.getOperator();

        if (left instanceof BooleanCrystal booleanCrystal) {
            boolean isTrue = booleanCrystal.getValue();
            if (operator instanceof LogicalAndOperatorCrystal) {
                if (!isTrue) return left;
            } else if (operator instanceof LogicalOrOperatorCrystal) {
                if (isTrue) return left;
            }
        }

        return evaluate(expr.getRight());
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
        //       (Once StringCrystals are implemented in Vikari.)
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
    public AtonementCrystal visit(VariableExpression expr) {
        AtonementCrystal reference = expr.getReference();
        String identifier = reference.getIdentifier();

        if (currentEnvironment.isDefined(identifier)) {
            AtonementCrystal value = currentEnvironment.get(identifier);
            return value;
        }

        throw internalRuntimeErrorForUndefinedVariable(reference);
    }

    @Override
    public AtonementCrystal visit(VariableDeclarationStatement stmt) {
        // Evaluate the initializer expression.
        Expression initializerExpression = stmt.getInitializerExpression();
        AtonementCrystal initialValue = null;
        if (initializerExpression != null) {
            initialValue = evaluate(stmt.getInitializerExpression());
        }

        // Create the crystal to be stored in the environment.
        AtonementCrystal reference = stmt.getDeclaredVariable();
        String identifier = reference.getIdentifier();

        AtonementCrystal variableToDefine = initializeVariableDeclaration(initialValue, identifier, stmt.getDeclaredType());

        // Store the crystal in the environment.
        if (!currentEnvironment.isDefined(identifier)) {
            currentEnvironment.define(identifier, variableToDefine);
        } else {
            throw internalRuntimeErrorForRedeclaredVariable(reference);
        }

        return variableToDefine;
    }

    private AtonementCrystal initializeVariableDeclaration(AtonementCrystal value, String identifier,
                                                           TypeCrystal declaredType) {
        AtonementCrystal declaredVariable;

        if (value instanceof NumberCrystal<?> number) {
            declaredVariable = Arithmetic.maybeUpcastOrDowncast(number, declaredType);
        } else if (value == null || value instanceof NullKeywordCrystal) {
            declaredVariable = new NullCrystal(0);
        } else {
            declaredVariable = value.copy();
        }

        declaredVariable.setIdentifier(identifier);
        declaredVariable.setDeclaredType(declaredType);

        // NOTE: The initializedType is already set! (Unless it is a NullCrystal.)
        if (declaredVariable instanceof NullCrystal) {
            TypeCrystal nullType = TypeHierarchy.getNullTypeFor(declaredType);
            declaredVariable.setInstantiatedType(nullType);
        }

        return declaredVariable;
    }

    private AtonementCrystal initializeVariableAssignment(AtonementCrystal value, String identifier,
                                                          TypeCrystal declaredType) {
        AtonementCrystal variable;

        if (value instanceof NumberCrystal<?> number) {
            variable = Arithmetic.maybeUpcastOrDowncast(number, declaredType);
        } else if (value instanceof NullKeywordCrystal) {
            variable = new NullCrystal(0);
        } else {
            variable = value.copy();
        }

        variable.setIdentifier(identifier);
        variable.setDeclaredType(declaredType);

        // NOTE: The initializedType is already set! (Unless it is a NullCrystal.)
        if (variable instanceof NullCrystal) {
            TypeCrystal nullType = TypeHierarchy.getNullTypeFor(declaredType);
            variable.setInstantiatedType(nullType);
        }

        return variable;
    }

    @Override
    public AtonementCrystal visit(LeftAssignmentExpression expr) {
        AtonementCrystal rvalue = evaluate(expr.getRvalue());
        AtonementCrystal lvalue = evaluate(expr.getLvalue());

        String identifier = lvalue.getIdentifier();
        if (currentEnvironment.isDefined(identifier)) {
            AtonementCrystal currentValue = currentEnvironment.get(identifier);
            TypeCrystal declaredType = currentValue.getDeclaredType();

            AtonementCrystal variableToAssign = initializeVariableAssignment(rvalue, identifier, declaredType);
            currentEnvironment.assign(identifier, variableToAssign);

            return variableToAssign;
        }
        throw internalRuntimeErrorForUndefinedVariable(lvalue);
    }

    @Override
    public AtonementCrystal visit(RightAssignmentExpression expr) {
        AtonementCrystal rvalue = evaluate(expr.getRvalue());
        AtonementCrystal lvalue = evaluate(expr.getLvalue());

        String identifier = lvalue.getIdentifier();
        if (currentEnvironment.isDefined(identifier)) {
            AtonementCrystal currentValue = currentEnvironment.get(identifier);
            TypeCrystal declaredType = currentValue.getDeclaredType();

            AtonementCrystal variableToAssign = initializeVariableAssignment(rvalue, identifier, declaredType);
            currentEnvironment.assign(identifier, variableToAssign);

            return variableToAssign;
        }
        throw internalRuntimeErrorForUndefinedVariable(lvalue);
    }

    @Override
    public AtonementCrystal visit(NullLiteralExpression expr) {
        AtonementCrystal expressionResult = evaluate(expr.getExpression());

        if (expressionResult instanceof NumberCrystal<?> numberCrystal) {
            TypeCrystal integerType = VikariType.INTEGER.getTypeCrystal();
            IntegerCrystal integerCrystal = (IntegerCrystal) Arithmetic.maybeUpcastOrDowncast(numberCrystal, integerType);
            int length = integerCrystal.getValue();
            return new NullCrystal(length);
        } else {
            CoordinatePair errorLocation = expr.getExpression().getLocation();
            throw internalRuntimeError(errorLocation, "Null literal expression expects a Number as its operand.");
        }
    }

    @Override
    public AtonementCrystal visit(UnaryExpression expr) {
        AtonementCrystal operand = evaluate(expr.getOperand());
        UnaryOperatorCrystal operator = expr.getOperator();
        AtonementCrystal result = operator.evaluate(operand);

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

    private Vikari_RuntimeException internalRuntimeError(CoordinatePair location, String errorMessage) {
        int lineNumber = location.getRow();
        String line = getLineFunction.apply(currentFile, lineNumber);
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

    private Vikari_RuntimeException internalRuntimeErrorForRedeclaredVariable(AtonementCrystal operator) {
        CoordinatePair location = operator.getCoordinates();
        String errorMessage = "Redeclared variable: ``" + operator.getIdentifier() + "``.";
        return internalRuntimeError(location, errorMessage);
    }

    private Vikari_RuntimeException internalRuntimeErrorForUndefinedVariable(AtonementCrystal operator) {
        CoordinatePair location = operator.getCoordinates();
        String errorMessage = "Undefined variable: ``" + operator.getIdentifier() + "``.";
        return internalRuntimeError(location, errorMessage);
    }

    public void clear() {
        currentFile = null;
        rootEnvironment = null;
        currentEnvironment = null;
    }
}
