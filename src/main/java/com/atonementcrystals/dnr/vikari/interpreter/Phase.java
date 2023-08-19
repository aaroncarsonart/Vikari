package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.error.Vikari_Exception;

/**
 * For determining which phases of the interpreter to run.
 */
public enum Phase {
    LEX, PARSE, EXECUTE, DEFAULT;

    public String getFullOptionArgumentString() {
        if (this == DEFAULT) {
            throw new Vikari_Exception("Internal Error", DEFAULT.name() + " has no associated option.");
        }

        return "--" + name().substring(0, 1).toUpperCase() + name().substring(1).toLowerCase();
    }
}
