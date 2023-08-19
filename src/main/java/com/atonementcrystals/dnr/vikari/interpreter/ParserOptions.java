package com.atonementcrystals.dnr.vikari.interpreter;

import java.util.ArrayList;
import java.util.List;

public class ParserOptions {
    public final boolean printAst;           // p
    public final boolean statementNumbers;   // n
    public final boolean verbose;            // v

    public ParserOptions(boolean printAst, boolean statementNumbers, boolean verbose) {
        this.printAst = printAst;
        this.statementNumbers = statementNumbers;
        this.verbose = verbose;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("ParserOptions{");

        List<String> enabledFlags = new ArrayList<>();
        if (printAst) enabledFlags.add("printAst");
        if (statementNumbers) enabledFlags.add("statementNumbers");
        if (verbose) enabledFlags.add("verbose");

        String csv = String.join(",", enabledFlags);
        sb.append(csv);
        sb.append('}');

        return sb.toString();
    }
}
