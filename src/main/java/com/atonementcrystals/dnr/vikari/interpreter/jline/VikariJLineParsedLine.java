package com.atonementcrystals.dnr.vikari.interpreter.jline;

import org.jline.reader.CompletingParsedLine;
import org.jline.reader.ParsedLine;

import java.util.List;

/**
 * A simple implementation to silence log warnings.
 */
public class VikariJLineParsedLine implements ParsedLine, CompletingParsedLine {

    private String line;

    public VikariJLineParsedLine(String line) {
        this.line = line;
    }

    @Override
    public CharSequence escape(CharSequence candidate, boolean complete) {
        return null;
    }

    @Override
    public int rawWordCursor() {
        return 0;
    }

    @Override
    public int rawWordLength() {
        return 0;
    }

    @Override
    public String word() {
        return line;
    }

    @Override
    public int wordCursor() {
        return 0;
    }

    @Override
    public int wordIndex() {
        return 0;
    }

    @Override
    public List<String> words() {
        return List.of(line);
    }

    @Override
    public String line() {
        return line;
    }

    @Override
    public int cursor() {
        return 0;
    }
}
