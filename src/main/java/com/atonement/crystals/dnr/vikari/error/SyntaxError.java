package com.atonement.crystals.dnr.vikari.error;

import com.atonement.crystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

public class SyntaxError extends VikariError {
    public SyntaxError(File file, CoordinatePair location, String line, String message) {
        super(file, location, line, message);
    }
}
