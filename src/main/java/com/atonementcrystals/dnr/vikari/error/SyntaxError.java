package com.atonementcrystals.dnr.vikari.error;

import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

public class SyntaxError extends VikariError {
    public SyntaxError(File file, CoordinatePair location, String line, String message) {
        super(file, location, line, message);
    }
}
