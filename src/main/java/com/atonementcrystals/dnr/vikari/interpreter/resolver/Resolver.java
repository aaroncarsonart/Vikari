package com.atonementcrystals.dnr.vikari.interpreter.resolver;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Resolvers optionally return a value of type T after walking the AST output of
 * the Parser and visiting each encountered Statement and Expression. They also
 * report a number of resolution errors to be retrieved after a call to resolve()
 * based on what the Resolver is resolving.
 *
 * @param <T> The type of value returned by the Resolver.
 */
public abstract class Resolver<T> implements Statement.Visitor<T>, Expression.Visitor<T> {
    protected List<ResolverError> resolutionErrors = new ArrayList<>();

    public void resolve(List<Statement> statements) {
        for (Statement statement : statements) {
            statement.accept(this);
        }
    }

    public final List<ResolverError> getResolutionErrors() {
        return resolutionErrors;
    }

    public final void reportErrors(SyntaxErrorReporter syntaxErrorReporter, File file) {
        for (ResolverError resolutionError : resolutionErrors) {
            CoordinatePair location = resolutionError.getLocation();
            String errorMessage = resolutionError.getErrorMessage();

            SyntaxError syntaxError = new SyntaxError(file, location, errorMessage);
            syntaxErrorReporter.add(syntaxError);
        }
    }
    public final void error(AtonementCrystal crystal, String errorMessage) {
        error(crystal.getCoordinates(), errorMessage);
    }

    public final void error(CoordinatePair location, String errorMessage) {
        ResolverError resolverError = new ResolverError(location, errorMessage);
        resolutionErrors.add(resolverError);
    }
}
