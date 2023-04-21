package com.atonement.crystals.dnr.vikari.interpreter;

/**
 * For determining which phases of the interpreter to run.
 */
public enum Phase {
    LEX("Lexer"),
    PARSE("Parser"),
    EXECUTE("Execute"),
    DEFAULT(null);

    private String name;

    Phase(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
