package com.atonementcrystals.dnr.vikari.error;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SyntaxErrorReporter {
    private static final Logger log = LogManager.getLogger(SyntaxErrorReporter.class);

    private final List<VikariError> syntaxErrors;
    private final List<VikariError> compilationWarnings;
    private final Map<File, List<String>> fileToLinesMap;

    public SyntaxErrorReporter() {
        this.syntaxErrors = new ArrayList<>();
        this.compilationWarnings = new ArrayList<>();
        this.fileToLinesMap = new HashMap<>();
    }

    public List<VikariError> getSyntaxErrors() {
        return syntaxErrors;
    }

    public List<VikariError> getCompilationWarnings() {
        return compilationWarnings;
    }

    public void add(VikariError syntaxError) {
        setLineFromCache(syntaxError);
        this.syntaxErrors.add(syntaxError);
    }

    public void add(CompilationWarning compilationWarning) {
        setLineFromCache(compilationWarning);
        this.compilationWarnings.add(compilationWarning);
    }

    private void setLineFromCache(VikariError vikariError) {
        File file = vikariError.getFile();
        int lineNumber = vikariError.getLocation().getRow();
        String line = getLineFromCache(file, lineNumber);
        vikariError.setLine(line);
    }

    public boolean hasErrors() {
        return !syntaxErrors.isEmpty();
    }

    public boolean hasWarnings() {
        return !compilationWarnings.isEmpty();
    }

    public String getVikariErrorReport(List<VikariError> vikariErrors, String errorReportLabel) {
        vikariErrors.sort(Comparator.comparing(VikariError::getLocation));

        StringBuilder sb = new StringBuilder();

        errorReportLabel += ":";
        String lineHeader = "-".repeat(errorReportLabel.length());

        sb.append(lineHeader).append('\n');
        sb.append(errorReportLabel).append('\n');
        sb.append(lineHeader).append('\n');

        for (int i = 0; i < vikariErrors.size(); i++) {
            VikariError vikariError = vikariErrors.get(i);
            String errorReport = vikariError.getErrorReport();
            sb.append(errorReport);
            sb.append('\n');

            // Separate entries by an additional newline.
            if (i < vikariErrors.size() - 1) {
                sb.append("\n");
            }
        }

        String vikariErrorReport = sb.toString();
        return vikariErrorReport;
    }

    public void reportSyntaxErrors() {
        String syntaxErrorReport = getVikariErrorReport(syntaxErrors, "Syntax Errors");
        System.out.println(syntaxErrorReport);
        log.debug("Error report:\n{}", syntaxErrorReport);
    }

    public void reportWarnings() {
        String compilationWarningReport = getVikariErrorReport(compilationWarnings, "Compilation Warnings");
        System.out.println(compilationWarningReport);
        log.debug("Warning report:\n{}", compilationWarningReport);
    }

    public void clear() {
        syntaxErrors.clear();
        compilationWarnings.clear();
    }

    public List<String> getLineCacheFor(File file) {
        return fileToLinesMap.computeIfAbsent(file, f -> new ArrayList<>());
    }

    public String getLineFromCache(File file, int lineNumber) {
        if (fileToLinesMap.containsKey(file)) {
            List<String> lines = fileToLinesMap.get(file);
            return lines.get(lineNumber);
        }
        throw new SyntaxErrorReportingException("Internal Error: No line data mapped to file: " +
                "\"" + file.getAbsolutePath() + "\"");
    }
}
