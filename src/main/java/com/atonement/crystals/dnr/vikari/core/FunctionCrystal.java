package com.atonement.crystals.dnr.vikari.core;

import java.util.List;

public class FunctionCrystal extends AtonementCrystal {

    private List<AtonementCrystal> parameters;
    private List<Statement> statements;
    private TypeCrystal returnType;

    public FunctionCrystal(String functionName) {
        super(functionName);
    }
}
