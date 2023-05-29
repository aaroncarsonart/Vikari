package com.atonementcrystals.dnr.vikari.interpreter.resolver;

import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

public class ResolverError {
    private CoordinatePair location;
    private String errorMessage;

    public ResolverError(CoordinatePair location, String errorMessage) {
        this.location = location;
        this.errorMessage = errorMessage;
    }

    public CoordinatePair getLocation() {
        return location;
    }

    public String getErrorMessage() {
        return errorMessage;
    }
}
