package com.atonement.crystals.dnr.vikari.interpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class LexerOptions {
    public final boolean printLineNumbers;
    public final boolean showInvisibles;
    public final boolean separateTokens;
    public final boolean verbose;

    public LexerOptions() {
        printLineNumbers = true;
        showInvisibles = true;
        separateTokens = true;
        verbose = true;
    }

    public LexerOptions(boolean printLineNumbers, boolean showInvisibles,
                        boolean separateTokens, boolean verbose) {
        this.printLineNumbers = printLineNumbers;
        this.showInvisibles = showInvisibles;
        this.separateTokens = separateTokens;
        this.verbose = verbose;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("LexerOptions{");

        List<String> enabledFlags = new ArrayList<>();
        if (printLineNumbers) enabledFlags.add("printLineNumbers");
        if (showInvisibles) enabledFlags.add("showInvisibles");
        if (separateTokens) enabledFlags.add("separateTokens");
        if (verbose) enabledFlags.add("verbose");

        String csv = enabledFlags.stream().collect(Collectors.joining(","));
        sb.append(csv);
        sb.append('}');

        return sb.toString();
    }
}
