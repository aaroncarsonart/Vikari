package com.atonementcrystals.dnr.vikari.interpreter;

import java.util.ArrayList;
import java.util.List;

public class LexerOptions {
    public final boolean printTokens;        // p
    public final boolean statementNumbers;   // n
    public final boolean showInvisibles;     // i
    public final boolean separateTokens;     // t
    public final boolean verbose;            // v
    public boolean warnings;                 // w

    public LexerOptions(boolean printTokens, boolean statementNumbers, boolean showInvisibles,
                        boolean separateTokens, boolean verbose, boolean warnings) {
        this.printTokens = printTokens;
        this.statementNumbers = statementNumbers;
        this.showInvisibles = showInvisibles;
        this.separateTokens = separateTokens;
        this.verbose = verbose;
        this.warnings = warnings;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("LexerOptions{");

        List<String> enabledFlags = new ArrayList<>();
        if (printTokens) enabledFlags.add("printTokens");
        if (statementNumbers) enabledFlags.add("statementNumbers");
        if (showInvisibles) enabledFlags.add("showInvisibles");
        if (separateTokens) enabledFlags.add("separateTokens");
        if (verbose) enabledFlags.add("verbose");
        if (warnings) enabledFlags.add("warnings");

        String csv = String.join(",", enabledFlags);
        sb.append(csv);
        sb.append('}');

        return sb.toString();
    }
}
