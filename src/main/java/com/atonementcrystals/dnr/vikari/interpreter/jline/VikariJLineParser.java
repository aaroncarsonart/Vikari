package com.atonementcrystals.dnr.vikari.interpreter.jline;

import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
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
    }

    @Override
    public ParsedLine parse(String line, int cursor, ParseContext context) throws org.jline.reader.SyntaxError {
        VikariJLineParsedLine parsedLine = new VikariJLineParsedLine(line);

        if (line.isEmpty() || context == ParseContext.COMPLETE) {
            return parsedLine;
        }

        lexer.lexToStringTokens(line);
        List<CoordinatePair> unclosedCommentLocations = getUnclosedCommentLocations();

        errorReporter.clear();
        lexer.resetTo(0);

        if (!unclosedCommentLocations.isEmpty()) {
            CoordinatePair firstErrorLocation = unclosedCommentLocations.get(0);
            int row = firstErrorLocation.getRow();
            int column = firstErrorLocation.getColumn();
            throw new EOFError(row, column, "Unclosed comment.", "comment", unclosedCommentLocations.size(), ":~");
        }

        return parsedLine;
    }

    private List<CoordinatePair> getUnclosedCommentLocations() {
        List<SyntaxError> allSyntaxErrors = errorReporter.getSyntaxErrors();
        List<CoordinatePair> unclosedCommentLocations = new ArrayList<>();

        for (SyntaxError syntaxError : allSyntaxErrors) {
            String errorMessage = syntaxError.getMessage();
            if (errorMessage.contains("Missing comment suffix token")) {
                CoordinatePair errorLocation = syntaxError.getLocation();
                unclosedCommentLocations.add(errorLocation);
            }
        }

        return unclosedCommentLocations;
    }
}
