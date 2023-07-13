package com.atonementcrystals.dnr.vikari.interpreter.jline;

import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import com.atonementcrystals.dnr.vikari.util.Utils;
import org.jline.reader.EOFError;
import org.jline.reader.ParsedLine;
import org.jline.reader.Parser;

import java.util.ArrayList;
import java.util.List;

/**
 * Allow for some Vikari statements to be defined across multiple lines in JLine during a call to LineReader::readLine.
 */
public class VikariJLineParser implements Parser {
    private final Lexer lexer;
    private final SyntaxErrorReporter errorReporter;

    public VikariJLineParser() {
        lexer = new Lexer();
        errorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(errorReporter);
        lexer.setCompilationWarningsEnabled(true);
    }

    @Override
    public ParsedLine parse(String line, int cursor, ParseContext context) throws org.jline.reader.SyntaxError {
        VikariJLineParsedLine parsedLine = new VikariJLineParsedLine(line);

        if (line.isEmpty() || context == ParseContext.COMPLETE) {
            return parsedLine;
        }

        lexer.lex(line);

        // Accept a new line of input if the statement is terminated by an unclosed comment token.
        List<CoordinatePair> unclosedCommentLocations = getUnclosedCommentLocations();
        if (!unclosedCommentLocations.isEmpty()) {
            reset();

            CoordinatePair firstErrorLocation = unclosedCommentLocations.get(0);
            int row = firstErrorLocation.getRow();
            int column = firstErrorLocation.getColumn();
            throw new EOFError(row, column, "Unclosed comment.", "comment", unclosedCommentLocations.size(), ":~");
        }

        // Accept a new line of input if the statement is terminated by a line continuation.
        CoordinatePair terminatingLineContinuationLocation = lexer.getTerminatingLineContinuationLocation();
        if (terminatingLineContinuationLocation != null) {
            reset();

            int row = terminatingLineContinuationLocation.getRow();
            int finalLineNumber = Utils.countOccurrences(line, '\n');

            // Ignore line continuation if the next line is blank.
            if (finalLineNumber > row) {
                return parsedLine;
            }

            int column = terminatingLineContinuationLocation.getColumn();
            throw new EOFError(row, column, "Line continuation.", "~", 0, "");
        }

        reset();
        return parsedLine;
    }

    private void reset() {
        errorReporter.clear();
        lexer.resetTo(0);
    }

    private List<CoordinatePair> getUnclosedCommentLocations() {
        List<VikariError> allSyntaxErrors = errorReporter.getSyntaxErrors();
        List<CoordinatePair> unclosedCommentLocations = new ArrayList<>();

        for (VikariError syntaxError : allSyntaxErrors) {
            String errorMessage = syntaxError.getMessage();
            if (errorMessage.contains("Missing comment suffix token")) {
                CoordinatePair errorLocation = syntaxError.getLocation();
                unclosedCommentLocations.add(errorLocation);
            }
        }

        return unclosedCommentLocations;
    }
}
