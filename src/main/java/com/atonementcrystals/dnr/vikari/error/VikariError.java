package com.atonementcrystals.dnr.vikari.error;

import com.atonementcrystals.dnr.vikari.util.Utils;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;

import java.io.File;
import java.util.Formatter;

public abstract class VikariError {
    private File file;
    private CoordinatePair location;
    private String line;
    private String message;

    public VikariError(File file, CoordinatePair location, String line, String message) {
        this.file = file;
        this.location = location;
        this.line = line;
        this.message = message;
    }

    public VikariError(File file, CoordinatePair location, String message) {
        this.file = file;
        this.location = location;
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

    public void setLine(String line) {
        this.line = line;
    }

    public String getMessage() {
        return message;
    }

    public String getErrorReport() {
        StringBuilder sb = new StringBuilder();
        Formatter formatter = new Formatter(sb);

        String filename;
        if (file == null) {
            filename = "<repl>";
        } else {
            filename = Utils.getShortenedFilename(file);
        }

        int row = location.getRow();
        int column = location.getColumn();

        String tab = "\t";
        String tabReplacement = "    ";

        // Need to sanitize code lines containing tab characters.
        int tabCount = Utils.countOccurrences(line, tab, column);
        line = line.replaceAll(tab, tabReplacement);

        // Add 1 to row and column because counting in error report should start from 1.
        formatter.format("%s:%d:%d:\n", filename, row + 1, column + 1);
        formatter.format("    %s\n", line);

        // Need to offset the caret placement based on the sanitized tabs.
        int tabOffset = tabCount * (tabReplacement.length() - 1);
        int caretOffset = column + 1 + tabOffset;

        formatter.format("    %" + caretOffset + "s\n", "^");
        formatter.format("    %s", message);

        return sb.toString();
    }
}
