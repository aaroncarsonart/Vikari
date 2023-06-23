package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
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
    private REPL_Interpreter interpreter;
    private SyntaxErrorReporter syntaxErrorReporter;

    private boolean exit;
    private LineReader userInput;

    /**
     * Instantiate a new VikariRepl instance.
     */
    public VikariREPL() {
       init();
    }

    private void init() {
        lexer = new Lexer();
        parser = new Parser();
        interpreter = new REPL_Interpreter();

        AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();
        parser.setGlobalAtonementField(globalAtonementField);
        interpreter.setGlobalAtonementField(globalAtonementField);
        interpreter.establishRootEnvironment();

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLineFromCache);
    }

    /**
     * Special REPL commands which when preceded by ! execute unique functionality
     * to drive the behavior of the REPL mode.
     */
    public enum ReplCommand {
        NONE, HELP, VERBOSE, CLEAR, EXIT, QUIT;

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
        userInput = LineReaderBuilder.builder().build();

        // Begin REPL loop.
        while (!exit) {
            try {
                // Get next line of user input.
                String nextLineOfUserInput = userInput.readLine(REPL_PROMPT);
                lexParseAndInterpret(nextLineOfUserInput);
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
     * Execute one iteration of the REPL loop body on the next line of user input.
     * @param nextLineOfUserInput The string to lex, parse, and interpret as Vikari code,
     *                            or else to read as a REPL command if it begins with !.
     */
    public void lexParseAndInterpret(String nextLineOfUserInput) {
        if (nextLineOfUserInput.isBlank()) {
            return;
        }

        // Parse REPL commands.
        ReplCommand replCommand = getReplCommand(nextLineOfUserInput);
        if (replCommand == ReplCommand.NONE) {
            System.out.println("Unrecognized REPL command.");
            return;
        } else if (replCommand != null) {
            executeReplCommand(replCommand);
            return;
        }

        // Lex and parse as Vikari code statement(s).
        List<List<AtonementCrystal>> lexedStatements = lexer.lex(nextLineOfUserInput);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        // Report syntax errors, if any.
        if (syntaxErrorReporter.hasErrors()) {
            reportSyntaxErrors(nextLineOfUserInput);
            syntaxErrorReporter.clear();
            return;
        }

        // Execute the Vikari code statement(s).
        try {
            log.trace("interpret()");

            for (Statement statement : parsedStatements) {
                interpreter.execute(statement);
            }
        }

        // Report runtime errors.
        catch (Vikari_RuntimeException e) {
            reportRuntimeError(e);
        }
    }

    private void reportVariableDeclaration(VariableDeclarationStatement statement, AtonementCrystal result) {
        AtonementCrystal declaredVariable = statement.getDeclaredVariable();
        String declaredVariableIdentifier = declaredVariable.getIdentifier();
        String initializedValue;
        if (result == null) {
            initializedValue = declaredVariable.getStringRepresentation();
        } else {
            initializedValue = result.getStringRepresentation();
        }
        System.out.printf("%s = %s\n", declaredVariableIdentifier, initializedValue);
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
            case VERBOSE:
                toggleVerbose();
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
        formatter.format("!%-6s Toggle results of declarations, assignments, and expressions.\n", ReplCommand.VERBOSE);
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
    public void clearReplState() {
        init();
    }

    public void toggleVerbose() {
        boolean enabled = interpreter.toggleVerboseOutput();
        if (enabled) {
            System.out.println("Verbose output mode enabled.");
        } else {
            System.out.println("Verbose output mode disabled.");
        }
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
