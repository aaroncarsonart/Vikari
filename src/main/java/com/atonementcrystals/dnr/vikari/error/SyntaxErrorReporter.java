package com.atonementcrystals.dnr.vikari.error;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Formatter;
import java.util.List;

public class SyntaxErrorReporter {
    private static final Logger log = LogManager.getLogger(SyntaxErrorReporter.class);

    private List<SyntaxError> syntaxErrors;

    public SyntaxErrorReporter() {
        this.syntaxErrors = new ArrayList<>();
    }

    public List<SyntaxError> getSyntaxErrors() {
        return syntaxErrors;
    }

    public void add(SyntaxError syntaxError) {
        this.syntaxErrors.add(syntaxError);
    }

    public boolean hasErrors() {
        return !syntaxErrors.isEmpty();
    }

    public String getErrorReport() {
        syntaxErrors.sort(Comparator.comparing(SyntaxError::getLocation));

        StringBuilder sb = new StringBuilder();

        sb.append("--------------\n");
        sb.append("Syntax Errors:\n");
        sb.append("--------------\n");

        for (int i = 0; i < syntaxErrors.size(); i++) {
            SyntaxError syntaxError = syntaxErrors.get(i);
            String errorReport = syntaxError.getErrorReport();
            sb.append(errorReport);
            sb.append('\n');

            // Separate entries by an additional newline.
            if (i < syntaxErrors.size() - 1) {
                sb.append("\n");
            }
        }

        String syntaxErrorReport = sb.toString();
        return syntaxErrorReport;
    }

    public void reportErrors() {
        String syntaxErrorReport = getErrorReport();
        System.out.println(syntaxErrorReport);
        log.debug("Error report:\n{}", syntaxErrorReport);
    }

    public void clear() {
        syntaxErrors.clear();
    }
}
