package com.atonementcrystals.dnr.vikari.error;

import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

public class CompilationWarning extends VikariError {

    public CompilationWarning(File file, CoordinatePair location, String message) {
        super(file, location, message);
    }

}
