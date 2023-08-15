package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.RuntimeError;
import com.atonementcrystals.dnr.vikari.error.VikariError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.Vikari_RuntimeException;
import com.atonementcrystals.dnr.vikari.interpreter.jline.VikariJLineParser;
import com.atonementcrystals.dnr.vikari.interpreter.resolver.TypeResolver;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jline.reader.EndOfFileException;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.UserInterruptException;
import org.jline.reader.impl.completer.NullCompleter;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;

/**
 * Handle all processing of the Vikari REPL mode.
 */
public class VikariREPL {
    private static final Logger log = LogManager.getLogger(VikariREPL.class);

    private static final String REPL_PROMPT = "vikari> ";
    private static final String REPL_SECONDARY_PROMPT = "......> ";
    private static final String REPL_COMMAND_PREFIX = "!";
    private static final String NEWLINE = "\n";
    private static final char SPACE = ' ';
    private static final char TAB = '\t';

    private Lexer lexer;
    private Parser parser;
    private REPL_Interpreter interpreter;
    private SyntaxErrorReporter syntaxErrorReporter;
    private TypeResolver typeResolver;

    private boolean exit;
    private boolean warningsEnabled;

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

        typeResolver = parser.getTypeResolver();
    }

    public void setWarningsEnabled(boolean warningsEnabled) {
        this.warningsEnabled = warningsEnabled;
        lexer.setCompilationWarningsEnabled(warningsEnabled);
    }

    /**
     * Special REPL commands which when preceded by ! execute unique functionality
     * to drive the behavior of the REPL mode.
     */
    public enum ReplCommand {
        NONE, HELP, VERBOSE, WARNINGS, CLEAR, EXIT, QUIT;

        private final String nameLowerCase;

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
        LineReader userInput = buildLineReader();

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
     * @return A fully-initialized LineReader for the REPL.
     */
    private LineReader buildLineReader() {
        return LineReaderBuilder.builder()
                .parser(new VikariJLineParser())
                .completer(NullCompleter.INSTANCE)
                .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
                .variable(LineReader.SECONDARY_PROMPT_PATTERN, REPL_SECONDARY_PROMPT)
                .build();
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

        int lexerLineNumber = lexer.getLineNumber();
        int parserLineNumber = parser.getLineNumber();

        // Lex and parse as Vikari code statement(s).
        List<List<AtonementCrystal>> lexedStatements = lexer.lex(nextLineOfUserInput);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        // Report syntax errors, if any. (And compilation warnings if they are enabled.)
        if (syntaxErrorReporter.hasErrors() || (warningsEnabled && syntaxErrorReporter.hasWarnings())) {
            reportProblems(nextLineOfUserInput);

            // Clear any previous erroneous state.
            syntaxErrorReporter.clear();
            typeResolver.clear();
            lexer.resetTo(lexerLineNumber);
            parser.resetTo(parserLineNumber);
            undefineNewVariablesDeclared(parsedStatements);

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
            case HELP -> printHelp();
            case CLEAR -> clearReplState();
            case VERBOSE -> toggleVerbose();
            case WARNINGS -> toggleWarnings();
            case EXIT, QUIT -> exitReplMode();
            default -> throw new IllegalStateException("Unsupported REPL command: " + replCommand.name());
        }
    }

    /**
     * Undefine all new variables declared in the given statements.
     * @param statements The statements to undefine new variable declarations for.
     */
    private void undefineNewVariablesDeclared(List<Statement> statements) {
        for (Statement statement : statements) {
            if (statement instanceof VariableDeclarationStatement variableDeclarationStatement) {
                String identifier = variableDeclarationStatement.getDeclaredVariable().getIdentifier();
                AtonementField environment = variableDeclarationStatement.getEnvironment();

                // If the environment is null, then the variable already exists and was
                // not redefined because of a "Variable is already defined" error.
                if (environment != null && environment.isDefined(identifier)) {
                    environment.undefine(identifier);
                }
            }
        }
    }

    /**
     * Print the help menu to the terminal.
     */
    private void printHelp() {
        StringBuilder sb = new StringBuilder();
        Formatter formatter = new Formatter(sb);

        formatter.format("\n");
        formatter.format("!%-8s Print help menu.\n", ReplCommand.HELP);
        formatter.format("!%-8s Clear REPL state.\n", ReplCommand.CLEAR);
        formatter.format("!%-8s Toggle results of declarations, assignments, and expressions.\n", ReplCommand.VERBOSE);
        formatter.format("!%-8s Toggle compilation warnings.\n", ReplCommand.WARNINGS);
        formatter.format("!%-8s Exit REPL mode.\n", ReplCommand.EXIT);
        formatter.format("!%-8s Exit REPL mode.\n", ReplCommand.QUIT);
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

    public void toggleWarnings() {
        warningsEnabled = !warningsEnabled;
        lexer.setCompilationWarningsEnabled(warningsEnabled);

        if (warningsEnabled) {
            System.out.println("Warnings enabled.");
        } else {
            System.out.println("Warnings disabled.");
        }
    }

    /**
     * Exit the Vikari REPL mode.
     */
    private void exitReplMode() {
        exit = true;
    }

    /**
     * Report any syntax errors (and compilation warnings, if enabled) for the given line of user input.
     * @param line The line to report the errors and warnings for.
     */
    private void reportProblems(String line) {
        if (!syntaxErrorReporter.hasErrors() && !syntaxErrorReporter.hasWarnings()) {
            return;
        }

        List<VikariError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        List<VikariError> warnings = syntaxErrorReporter.getCompilationWarnings();

        for (VikariError syntaxError : syntaxErrors) {
            syntaxError.setMessage("Error: " + syntaxError.getMessage());
        }
        for (VikariError warning : warnings) {
            warning.setMessage("Warning: " + warning.getMessage());
        }

        List<VikariError> problems = new ArrayList<>();
        problems.addAll(syntaxErrors);
        problems.addAll(warnings);

        if (problems.size() == 1 && (!line.contains(NEWLINE) || problemAtEnd(problems.get(0)))) {
            VikariError problem = problems.get(0);
            reportSingleProblem(problem);
        } else {
            reportMultipleProblems(problems);
        }
    }

    private boolean problemAtEnd(VikariError problem) {
        int lastLineNumber = lexer.getLineNumber() - 1;
        int problemLineNumber = problem.getLocation().getRow();
        return lastLineNumber == problemLineNumber;
    }

    /**
     * Report a single VikariError in a shortened format. Taking advantage of the fact
     * that the user already typed the line containing the error. So only the caret ^
     * pointing to the location of the error and the error message need to be reported.
     * @param problem the VikariError to report.
     */
    private void reportSingleProblem(VikariError problem) {
        CoordinatePair location = problem.getLocation();
        int column = location.getColumn();
        String line = problem.getLine();

        // Build a string of spaces and tabs to ensure the caret ^ matches the location
        // of the VikariError in the line of user input just typed in the prompt.
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

        String problemMessage = problem.getMessage();
        System.out.println(problemMessage);

        // Trim the final trailing newline, which is not needed for the log entry.
        problemMessage = problemMessage.substring(0, problemMessage.length() - 1);

        // Print equivalent output as REPL in logs.
        log.debug("\n{}{}\n{}\n{}", REPL_PROMPT, line, caretStr, problemMessage);
    }

    /**
     * Report VikariErrors the same as SyntaxErrorReporter, but without the large
     * banner prefixing the error report.
     * @param problems The VikariErrors to report.
     */
    private void reportMultipleProblems(List<VikariError> problems) {
        StringBuilder sb = new StringBuilder();

        for (VikariError problem : problems) {
            String errorReport = problem.getErrorReport();
            sb.append(errorReport);
            sb.append(NEWLINE);
            sb.append(NEWLINE);
        }

        String fullErrorReport = sb.toString();
        System.out.print(fullErrorReport);

        // Trim the final trailing newline, which is not needed for the log entry.
        fullErrorReport = fullErrorReport.substring(0, fullErrorReport.length() - 1);

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
