package com.atonement.crystals.dnr.vikari.error;

import com.atonement.crystals.dnr.vikari.util.CoordinatePair;

import java.io.File;

public class SyntaxError {
    private File file;
    private CoordinatePair location;
    private String line;
    private String message;

    public SyntaxError(File file, CoordinatePair location, String line, String message) {
        this.file = file;
        this.location = location;
        this.line = line;
        this.message = message;
    }

    public File getFile() {
        return file;
    }

    public CoordinatePair getLocation() {
        return location;
    }

    public String getLine() {
        return line;
    }

    public String getMessage() {
        return message;
    }
}
