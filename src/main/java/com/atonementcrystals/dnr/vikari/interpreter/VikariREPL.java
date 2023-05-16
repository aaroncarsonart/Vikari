package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.PrintStatement;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.RuntimeError;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.Vikari_RuntimeException;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jline.reader.EndOfFileException;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.UserInterruptException;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;

/**
 * Handle all processing of the Vikari REPL mode.
 */
public class VikariREPL {
    private static final Logger log = LogManager.getLogger(VikariREPL.class);

    private static final String REPL_PROMPT = "vikari> ";
    private static final String REPL_COMMAND_PREFIX = "!";
    private static final String NEWLINE = "\n";
    private static final char SPACE = ' ';
    private static final char TAB = '\t';

    private Lexer lexer;
    private Parser parser;
    private TreeWalkInterpreter interpreter;
    private SyntaxErrorReporter syntaxErrorReporter;

    private List<String> lineHistory;
    private List<List<AtonementCrystal>> lexedStatements;
    private List<Statement> parsedStatements;
    private List<AtonementCrystal> interpretedResults;

    private boolean exit;
    private LineReader userInput;

    /**
     * Instantiate a new VikariRepl instance.
     */
    public VikariREPL() {
        lexer = new Lexer();
        parser = new Parser();
        interpreter = new TreeWalkInterpreter();

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        lineHistory = new ArrayList<>();
        lexedStatements = new ArrayList<>();
        parsedStatements = new ArrayList<>();
        interpretedResults = new ArrayList();

        userInput = LineReaderBuilder.builder().build();
    }

    /**
     * Special REPL commands which when preceded by ! execute unique functionality
     * to drive the behavior of the REPL mode.
     */
    public enum ReplCommand {
        NONE, HELP, CLEAR, EXIT, QUIT;

        private String nameLowerCase;

        ReplCommand() {
            nameLowerCase = name().toLowerCase();
        }

        public String getNameLowerCase() {
            return nameLowerCase;
        }
    }

    /**
     * Begin the Vikari interpreter in REPL mode.
     */
    public void start() {
        log.trace("start()");
        exit = false;

        // Begin REPL loop.
        while (!exit) {
            // TODO: Save program state in interpreter once assignment statements work.
            parser.clear();
            interpreter.clear();

            try {
                // Get next line of user input.
                String nextLineOfUserInput = userInput.readLine(REPL_PROMPT);

                if (nextLineOfUserInput.isBlank()) {
                    continue;
                }

                // Parse REPL commands.
                ReplCommand replCommand = getReplCommand(nextLineOfUserInput);
                if (replCommand == ReplCommand.NONE) {
                    System.out.println("Unrecognized Vikari REPL command.");
                    continue;
                } else if (replCommand != null) {
                    executeReplCommand(replCommand);
                    continue;
                }

                // Lex and parse as Vikari code statement(s).
                List<List<AtonementCrystal>> lexedStatements = lexer.lex(nextLineOfUserInput);
                List<Statement> parsedStatements = parser.parse(null, lexedStatements);

                lineHistory.add(nextLineOfUserInput);

                // Report syntax errors, if any.
                if (syntaxErrorReporter.hasErrors()) {
                    reportSyntaxErrors(nextLineOfUserInput);
                    syntaxErrorReporter.clear();
                    continue;
                }

                // Execute the Vikari code statement(s).
                try {
                    log.trace("interpret()");
                    List<AtonementCrystal> currentResults = new ArrayList<>();
                    for (Statement statement : parsedStatements) {
                        AtonementCrystal result = interpreter.execute(statement);
                        currentResults.add(result);
                    }

                    // TODO: Once the index operator is implemented, assign each bare
                    // TODO: ExpressionStatement result to $0, $1, $2 etc and report
                    // TODO: the resulting assignment as e.g. ``$0 << 7`` in the REPL.

                    // Cache the results if there were no runtime errors.
                    this.lexedStatements.addAll(lexedStatements);
                    this.parsedStatements.addAll(parsedStatements);
                    interpretedResults.addAll(currentResults);

                    // Report the output of the last statement.
                    if (!currentResults.isEmpty()) {
                        Statement lastStatement = parsedStatements.get(parsedStatements.size() - 1);
                        if (!(lastStatement instanceof PrintStatement)) {
                            AtonementCrystal lastResult = currentResults.get(currentResults.size() - 1);
                            System.out.println(lastResult.getStringRepresentation());
                        }
                    }
                }

                // Report runtime errors.
                catch (Vikari_RuntimeException e) {
                    reportRuntimeError(e);
                }
            }

            // Handle signal for Ctrl+C.
            catch (UserInterruptException e) {
                // Do nothing. (Just interrupt the present typed command.)
            }

            // Handle signal for Ctrl+D.
            catch (EndOfFileException e) {
                exitReplMode();
            }
        }
    }

    /**
     * Get a REPL command if it begins with the REPL command prefix "!" character.
     * Otherwise, this method returns null.
     * @param line The String to get the REPL command for.
     * @return The REPL command for the input string.
     */
    private ReplCommand getReplCommand(String line) {
        if (line.startsWith(REPL_COMMAND_PREFIX)) {
            String maybeCommand = line.substring(REPL_COMMAND_PREFIX.length()).toLowerCase();
            for (ReplCommand replCommand : ReplCommand.values()) {
                if (maybeCommand.equals(replCommand.getNameLowerCase())) {
                    return replCommand;
                }
            }
            return ReplCommand.NONE;
        }
        return null;
    }

    /**
     * Execute the given REPL command.
     * @param replCommand The REPL command to execute.
     */
    private void executeReplCommand(ReplCommand replCommand) {
        log.trace("executeReplCommand({})", replCommand);

        switch (replCommand) {
            case HELP:
                printHelp();
                break;
            case CLEAR:
                clearReplState();
                break;
            case EXIT:
            case QUIT:
                exitReplMode();
                break;
            default:
                throw new IllegalStateException("Unsupported REPL command: " + replCommand.name());
        }
    }

    /**
     * Print the help menu to the terminal.
     */
    private void printHelp() {
        StringBuilder sb = new StringBuilder();
        Formatter formatter = new Formatter(sb);

        formatter.format("\n");
        formatter.format("!%-6s Print help menu.\n", ReplCommand.HELP);
        formatter.format("!%-6s Clear REPL state.\n", ReplCommand.CLEAR);
        formatter.format("!%-6s Exit REPL mode.\n", ReplCommand.EXIT);
        formatter.format("!%-6s Exit REPL mode.\n", ReplCommand.QUIT);
        formatter.format("\n");
        formatter.format("Note that Vikari REPL commands are case-insensitive.\n");

        String result = sb.toString();
        System.out.print(result);
    }

    /**
     * Clear all cached program state.
     */
    private void clearReplState() {
        lexedStatements.clear();
        parsedStatements.clear();
        interpretedResults.clear();

        parser.clear();
        interpreter.clear();
    }

    /**
     * Exit the Vikari REPL mode.
     */
    private void exitReplMode() {
        exit = true;
    }

    /**
     * Report syntax errors for the given line of user input.
     * @param line The line to report the error for.
     */
    private void reportSyntaxErrors(String line) {
        if (!syntaxErrorReporter.hasErrors()) {
            return;
        }
        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        if (syntaxErrors.size() == 1 && !line.contains(NEWLINE)) {
            SyntaxError syntaxError = syntaxErrors.get(0);
            reportSingleSyntaxError(syntaxError);
        } else {
            reportMultipleSyntaxErrors(syntaxErrors);
        }
    }

    /**
     * Report a single SyntaxError in a shortened format. Taking advantage of the fact
     * that the user already typed the line containing the error. So only the caret ^
     * pointing to the location of the error and the error message need to be reported.
     * @param syntaxError the SyntaxError to report.
     */
    private void reportSingleSyntaxError(SyntaxError syntaxError) {
        CoordinatePair location = syntaxError.getLocation();
        int column = location.getColumn();
        String line = syntaxError.getLine();

        // Build a string of spaces and tabs to ensure the caret ^ matches the location
        // of the SyntaxError in the line of user input just typed in the prompt.
        StringBuilder sb = new StringBuilder();
        sb.append(REPL_PROMPT.replaceAll(".", " "));
        for (int i = 0; i < column; i++) {
            char c = line.charAt(i);
            if (c == TAB) {
                sb.append(TAB);
            } else {
                sb.append(SPACE);
            }
        }
        sb.append("^");

        String caretStr = sb.toString();
        System.out.println(caretStr);

        String errorMessage = syntaxError.getMessage();
        System.out.println(errorMessage);

        // Print equivalent output as REPL in logs.
        log.debug("\n{}{}\n{}", REPL_PROMPT, line, errorMessage);
    }

    /**
     * Report SyntaxErrors the same as SyntaxErrorReporter, but without the large
     * banner prefixing the error report.
     * @param syntaxErrors The SyntaxErrors to report.
     */
    private void reportMultipleSyntaxErrors(List<SyntaxError> syntaxErrors) {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < syntaxErrors.size(); i++) {
            SyntaxError syntaxError = syntaxErrors.get(i);
            String errorReport = syntaxError.getErrorReport();
            sb.append(errorReport);
            sb.append(NEWLINE);
            sb.append(NEWLINE);
        }

        String fullErrorReport = sb.toString();
        System.out.println(fullErrorReport);

        // Print equivalent output as REPL in logs.
        log.debug("\n{}", fullErrorReport);
    }

    /**
     * Report a RuntimeError.
     * @param e The exception containing the RuntimeError.
     */
    private void reportRuntimeError(Vikari_RuntimeException e) {
        RuntimeError runtimeError = e.getRuntimeError();
        String errorReport = runtimeError.getErrorReport();
        System.out.println(errorReport);

        // Print equivalent output as REPL in logs.
        log.debug("\n{}", errorReport);
    }
}
