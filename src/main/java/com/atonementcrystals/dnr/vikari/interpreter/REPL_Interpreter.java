package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LeftAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.expression.RightAssignmentExpression;
import com.atonementcrystals.dnr.vikari.core.statement.ExpressionStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Map;
import java.util.TreeMap;

/**
 * A subclass of TreeWalkInterpreter used to handle REPL-specific behavior for
 * reporting results of evaluating expressions and statements to the user.
 */
public class REPL_Interpreter extends TreeWalkInterpreter {
    private boolean verboseOutputEnabled = true;
    private final Map<String, String> assignmentResults = new TreeMap<>();

    public boolean toggleVerboseOutput() {
        verboseOutputEnabled = !verboseOutputEnabled;
        return verboseOutputEnabled;
    }

    /**
     * Cache the result for later reporting in the output of the REPL.
     * @param result The result to cache.
     */
    private void cacheAssignment(AtonementCrystal result) {
        String identifier = result.getIdentifier();

        // Ignore crystals not defined in the root environment.
        AtonementField rootEnvironment = getRootEnvironment();
        if (!rootEnvironment.hasFieldMember(identifier)) {
            return;
        }

        TypeCrystal type;
        if (result instanceof NullCrystal) {
            type = result.getDeclaredType();
        } else {
            type = result.getInstantiatedType();
        }

        String typeName = type.getTypeName();
        String resultRepresentation = result.getStringRepresentation();

        String assignmentReport = String.format("%s:%s = %s", identifier, typeName, resultRepresentation);
        assignmentResults.put(identifier, assignmentReport);
    }

    /**
     * Report the final results of all assignment expressions in the previously executed statement.
     */
    private void reportAssignments() {
        for (String assignmentResult : assignmentResults.values()) {
            System.out.println(assignmentResult);
        }
    }

    @Override
    public AtonementCrystal execute(Statement statement) {
        // TODO: Replace replOut with a cached list of StringCrystals output by PrintStatements.
        ByteArrayOutputStream replOut = new ByteArrayOutputStream();
        PrintStream systemOut = System.out;

        // Execute the statement, capturing all output in the buffer.
        System.setOut(new PrintStream(replOut));
        AtonementCrystal result = super.execute(statement);
        System.setOut(systemOut);

        // Print the output result.
        String output = replOut.toString();
        System.out.print(output);

        // Ensure the prompt begins on a new line.
        if (!output.isEmpty() && !output.endsWith("\n")) {
            System.out.println();
        }

        // Report assignments.
        if (verboseOutputEnabled) {
            reportAssignments();
        }

        // Clear the cache.
        assignmentResults.clear();

        return result;
    }

    @Override
    public AtonementCrystal visit(VariableDeclarationStatement stmt) {
        AtonementCrystal result = super.visit(stmt);
        cacheAssignment(result);
        return result;
    }

    @Override
    public AtonementCrystal visit(LeftAssignmentExpression expr) {
        AtonementCrystal result = super.visit(expr);
        cacheAssignment(result);
        return result;
    }

    @Override
    public AtonementCrystal visit(RightAssignmentExpression expr) {
        AtonementCrystal result = super.visit(expr);
        cacheAssignment(result);
        return result;
    }

    @Override
    public AtonementCrystal visit(ExpressionStatement stmt) {
        AtonementCrystal result = evaluate(stmt.getExpression());

        if (verboseOutputEnabled) {
            Expression expr = stmt.getExpression();
            if (!(expr instanceof LeftAssignmentExpression || expr instanceof RightAssignmentExpression)) {
                System.out.println(result.getStringRepresentation());
            }
        }

        return result;
    }
}
