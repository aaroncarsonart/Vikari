package com.atonement.crystals.dnr.vikari;

import com.atonement.crystals.dnr.vikari.core.statement.Statement;
import com.atonement.crystals.dnr.vikari.error.Vikari_TypeError;
import com.atonement.crystals.dnr.vikari.interpreter.LexerOptions;
import com.atonement.crystals.dnr.vikari.interpreter.ParserOptions;
import com.atonement.crystals.dnr.vikari.interpreter.Phase;
import com.atonement.crystals.dnr.vikari.interpreter.ProgramId;
import com.atonement.crystals.dnr.vikari.interpreter.VikariProgram;
import com.atonement.crystals.dnr.vikari.interpreter.VikariSourceFileLoader;
import com.atonement.crystals.dnr.vikari.util.Utils;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.LoggerConfig;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Properties;

/**
 * Lex, parse, and interpret Vikari programs based on input of command-line arguments.
 */
public class Main {
    private static final Logger log = LogManager.getLogger(Main.class);

    /**
     * Entry point of the program.
     * @param args All command-line arguments.
     */
    public static void main(String[] args) {
        runVikariProgram(args);
        log.debug("End of program.\n");
    }

    /**
     * Parse command-line arguments, and run the Vikari interpreter.
     * @param args All command-line arguments to parse.
     */
    public static void runVikariProgram(String[] args) {
        ProgramId.initialize();

        CommandLineParser commandLineParser = new DefaultParser();
        Options options = buildOptions();

        try {
            CommandLine cmd = commandLineParser.parse(options, args);

            //-----------------------------
            // 0. Set log level of project.
            //-----------------------------
            if (cmd.hasOption("log-level")) {
                String logLevelName = cmd.getOptionValue("log-level");
                Level logLevel = Level.getLevel(logLevelName);
                if (logLevel != null) {
                    setLogLevel(logLevel);
                    log.debug("runVikariProgram()");
                    log.debug("set log level to: {}.", logLevel);
                } else {
                    log.debug("runVikariProgram()");
                    String msg = "Unrecognized log level: ``" + logLevelName + "``";
                    log.debug(msg);
                    System.err.println(msg);
                    System.err.println("Supported values: ALL, TRACE, DEBUG, INFO, WARN, ERROR, FATAL, OFF.");
                    return;
                }
            } else {
                setLogLevel(Level.TRACE);
                log.debug("runVikariProgram()");
            }

            Phase phase = Phase.DEFAULT;
            String defaultConfigOptions = "litp";
            LexerOptions lexerOptions = parseLexerOptions(defaultConfigOptions);
            ParserOptions parserOptions = parseParserOptions(defaultConfigOptions);
            List<String> argsList = cmd.getArgList();

            // ------------------------------------------------------------------
            // 1. Run default behavior of interpreter with no optional arguments.
            // ------------------------------------------------------------------
            if (cmd.getOptions().length == 0) {
                log.debug("No options provided.");

                // Run the only argument as a file.
                if (argsList.size() == 1) {
                    log.debug("One argument. Default behavior.");
                    log.debug("Use default phase.");
                    log.debug("Use default config:\n    " + lexerOptions);
                    log.debug("Use file OR type source option.");

                    String pathToSourceFile = argsList.get(0);

                    // Attempt to resolve first argument as a fully-qualified type name.
                    if (Utils.validateFullyQualifiedTypeName(pathToSourceFile)) {
                        pathToSourceFile = Utils.filePathForTypeName(pathToSourceFile);
                    }

                    VikariSourceFileLoader sourceFileLoader = new VikariSourceFileLoader();
                    File sourceFile = sourceFileLoader.loadSourceFile(pathToSourceFile);
                    runSourceFile(sourceFile, phase, lexerOptions, parserOptions);
                    return;
                }

                // Run repl mode.
                else if (argsList.size() == 0) {
                    log.debug("Zero arguments. Default behavior.");
                    log.debug("Use default phase.");
                    log.debug("Use default config:\n    " + lexerOptions);

                    runReplMode(phase, lexerOptions);
                    return;
                }
            }

            // --------------------------------------------------------
            // 2. If too many arguments, print help message. Then exit.
            // --------------------------------------------------------
            if (argsList.size() > 1) {
                System.err.println("Too many arguments.");
                printHelp(options);
                return;
            }

            // -----------------------------------------------------
            // 3. Exit early if version or help option is requested.
            // -----------------------------------------------------
            if (cmd.hasOption("version")) {
                printVersion();
                return;
            }

            if (cmd.hasOption("help")) {
                printHelp(options);
                return;
            }

            // -----------------------------------------
            // 4. Determine phase of interpreter to run.
            // -----------------------------------------
            // Lexer phase.
            if (cmd.hasOption("lex")) {
                log.debug("Lexer phase selected.");
                log.debug("Use default config:\n    " + lexerOptions);
                phase = Phase.LEX;
            } else if (cmd.hasOption("Lex")) {
                log.debug("Lexer phase selected.");
                phase = Phase.LEX;

                String configArgument = cmd.getOptionValue("Lex");
                lexerOptions = parseLexerOptions(configArgument);
                log.debug("Use config:\n    {}\n", lexerOptions);
            }

            // Parser phase.
            else if (cmd.hasOption("parse")) {
                log.debug("Parser phase selected.");
                log.debug("Use default config:\n    " + lexerOptions);
                phase = Phase.PARSE;
            } else if (cmd.hasOption("Parse")) {
                log.debug("Parser phase selected.");
                phase = Phase.PARSE;

                String configArgument = cmd.getOptionValue("Parse");
                lexerOptions = parseLexerOptions(configArgument);
                parserOptions = parseParserOptions(configArgument);
                log.debug("Use config:\n    {}\n    {}\n", lexerOptions, parserOptions);
            }

            // Execute phase.
            else if (cmd.hasOption("execute")) {
                log.debug("Execute phase selected.");
                log.debug("Use default config:\n    " + lexerOptions);

                phase = Phase.EXECUTE;
            } else if (cmd.hasOption("Execute")) {
                log.debug("Execute phase selected.");
                phase = Phase.EXECUTE;

                String configArgument = cmd.getOptionValue("Execute");
                lexerOptions = parseLexerOptions(configArgument);
                parserOptions = parseParserOptions(configArgument);
                log.debug("Use config:\n    {}\n    {}\n", lexerOptions, parserOptions);
            }

            // No phase requested. Use default behavior.
            else {
                log.debug("No phase selected.");
                log.debug("Use default phase.");
                log.debug("Use default config:\n    " + lexerOptions);
            }

            // ----------------------------------------------
            // 5. Determine the source of the Vikari program.
            // ----------------------------------------------
            String pathToFile = null;
            String sourceString = null;
            boolean replMode = false;
            int expectedAdditionalArguments = 1;

            // Get path to file from option argument.
            if (cmd.hasOption("file")) {
                log.debug("File source option selected.");
                pathToFile = cmd.getOptionValue("file");
                log.debug("File: ``" + pathToFile +"``");
                expectedAdditionalArguments = 0;
            }

            // Get path to file from type or script name in option argument.
            else if (cmd.hasOption("type")) {
                log.debug("Type source option selected.");
                String fullyQualifiedTypeName = cmd.getOptionValue("type");
                boolean isValid = Utils.validateFullyQualifiedTypeName(fullyQualifiedTypeName);
                if (isValid) {
                    pathToFile = Utils.filePathForTypeName(fullyQualifiedTypeName);
                    log.debug("File: ``" + pathToFile + "``");
                } else {
                    String message = "Type name not resolvable. Use the following formats:\n" +
                            "``path::to::Type``\n" +
                            "``path::to::script``";
                    throw new Vikari_TypeError(message);
                }
                expectedAdditionalArguments = 0;
            }

            // Get code statements from option argument.
            else if (cmd.hasOption("code")) {
                log.debug("Code source option selected.");
                sourceString = cmd.getOptionValue("code");
                log.debug("Source code string: " + sourceString);
                expectedAdditionalArguments = 0;
            }

            // Run the repl mode for the interpreter.
            else if (cmd.hasOption("repl")) {
                log.debug("Repl source option selected.");
                replMode = true;
                expectedAdditionalArguments = 0;
            }

            // If no source option selected, use the final argument as a file OR type name.
            // (If present.)
            else {
                log.debug("No source option selected.");
                if (argsList.size() == 0) {
                    log.debug("Using repl mode.");
                    replMode = true;
                } else if (argsList.size() == 1) {
                    log.debug("Using file OR type mode.");
                    pathToFile = argsList.get(0);
                }
            }

            // If too many arguments, print help message. Then exit.
            if (argsList.size() > expectedAdditionalArguments) {
                System.err.println("Too many arguments.");
                printHelp(options);
                return;
            }

            // --------------------------------------
            // 6. Lex, parse, or execute the program.
            // --------------------------------------
            if (pathToFile != null) {
                VikariSourceFileLoader sourceFileLoader = new VikariSourceFileLoader();
                File sourceFile = sourceFileLoader.loadSourceFile(pathToFile);
                runSourceFile(sourceFile, phase, lexerOptions, parserOptions);
            } else if (sourceString != null) {
                runSourceString(sourceString, phase, lexerOptions, parserOptions);
            } else if (replMode) {
                runReplMode(phase, lexerOptions);
            }else {
                throw new IllegalStateException("Unreachable code.");
            }

        } catch (ParseException | Vikari_TypeError e) {
            String message = e.getMessage();
            log.debug(message);
            System.err.println(message);
        }
    }

    /**
     * Build all rules for parsing optional command-line arguments for the interpreter.
     * @return The Options object representing these optional argument rules.
     */
    public static Options buildOptions() {
        // Options are the characters: litv.
        Option lexOption = Option.builder("l").longOpt("lex")
                .required(false)
                .desc("Run lexer.")
                .build();

        Option parseOption = Option.builder("p").longOpt("parse")
                .required(false)
                .desc("Run lexer and parser.")
                .build();

        Option executeOption = Option.builder("e").longOpt("execute")
                .required(false)
                .desc("Run lexer, parser, and interpreter.")
                .build();

        Option lexWithArgumentOption = Option.builder("L").longOpt("Lex")
                .argName("config_options")
                .hasArg()
                .required(false)
                .desc("Run lexer with config options.")
                .build();

        Option parseWithArgumentOption = Option.builder("P").longOpt("Parse")
                .argName("config_options")
                .hasArg()
                .required(false)
                .desc("Run lexer and parser with config options.")
                .build();

        Option executeWithArgumentOption = Option.builder("E").longOpt("Execute")
                .argName("config_options")
                .hasArg()
                .required(false)
                .desc("Run lexer, parser, and interpreter with config options.")
                .build();

        OptionGroup interpreterPhaseOptions = new OptionGroup();
        interpreterPhaseOptions.addOption(lexOption);
        interpreterPhaseOptions.addOption(parseOption);
        interpreterPhaseOptions.addOption(executeOption);
        interpreterPhaseOptions.addOption(lexWithArgumentOption);
        interpreterPhaseOptions.addOption(parseWithArgumentOption);
        interpreterPhaseOptions.addOption(executeWithArgumentOption);

        Option fileOption = Option.builder("f").longOpt("file")
                .argName("file")
                .hasArg()
                .required(true)
                .desc("Provide a path to a source file.")
                .build();

        Option typeOption = Option.builder("t").longOpt("type")
                .argName("type")
                .hasArg()
                .required(true)
                .desc("Provide a fully-qualified type name.")
                .build();

        Option codeOption = Option.builder("c").longOpt("code")
                .desc("Run a string of Vikari source code.")
                .argName("code")
                .hasArg()
                .build();

        Option replOption = Option.builder("r").longOpt("repl")
                .required(false)
                .desc("Run repl mode.")
                .build();

        OptionGroup sourceLocationOptions = new OptionGroup();
        sourceLocationOptions.addOption(fileOption);
        sourceLocationOptions.addOption(typeOption);
        sourceLocationOptions.addOption(codeOption);
        sourceLocationOptions.addOption(replOption);

        Option versionNumberOption = Option.builder("v").longOpt("version")
                .desc("print version number")
                .build();

        Option helpOption = Option.builder("h").longOpt("help")
                .desc("show help dialog")
                .build();

        Option logLevelOption = Option.builder("g").longOpt("log-level")
                .desc("set log level")
                .argName("log_level")
                .hasArg()
                .required(false)
                .build();

        Options options = new Options();
        options.addOptionGroup(interpreterPhaseOptions);
        options.addOptionGroup(sourceLocationOptions);
        options.addOption(versionNumberOption);
        options.addOption(helpOption);
        options.addOption(logLevelOption);

        return options;
    }

    /**
     * Parse the LexerOptions from the {@literal <config_options>} argument.
     * @param optionsArgument The optional argument to parse.
     * @return The LexerOptions represented by the optional argument.
     */
    public static LexerOptions parseLexerOptions(String optionsArgument) {
        log.trace("parseLexerOptions()");
        log.trace("optionsArgument: {}", optionsArgument);

        boolean printLineNumbers = optionsArgument.contains("l");
        boolean showInvisibles = optionsArgument.contains("i");
        boolean separateTokens = optionsArgument.contains("t");
        boolean verbose = optionsArgument.contains("v");

        LexerOptions lexerOptions = new LexerOptions(
                printLineNumbers,
                showInvisibles,
                separateTokens,
                verbose);

        return lexerOptions;
    }

    /**
     * Parse the ParserOptions from the {@literal <config_options>} argument.
     * @param optionsArgument The optional argument to parse.
     * @return The ParserOptions represented by the optional argument.
     */
    public static ParserOptions parseParserOptions(String optionsArgument) {
        log.trace("parseParserOptions()");
        log.trace("optionsArgument: {}", optionsArgument);

        boolean printAst = optionsArgument.contains("p");
        boolean printLineNumbers = optionsArgument.contains("l");
        boolean verbose = optionsArgument.contains("v");
        ParserOptions parserOptions = new ParserOptions(printAst, printLineNumbers, verbose);
        return parserOptions;
    }

    /**
     * Run the given Vikari source file through the given phase with the given options.
     *
     * @param sourceFile The source file to interpret.
     * @param phase The phase of the interpreter to run the source file through.
     * @param lexerOptions An optional set of options for configuring output of the Lexer.
     */
    public static void runSourceFile(File sourceFile, Phase phase, LexerOptions lexerOptions, ParserOptions parserOptions) {
        log.debug("Run source file.");
        VikariProgram program = new VikariProgram();
        program.setLexerOptions(lexerOptions);
        program.setParserOptions(parserOptions);

        switch (phase) {
            case LEX:
                program.lex(sourceFile);
                program.reportSyntaxErrors();
                break;
            case PARSE:
                program.lexAndParse(sourceFile);
                program.reportSyntaxErrors();
                break;
            case EXECUTE:
            case DEFAULT:
                program.lexAndParse(sourceFile);
                if (program.hasErrors()) {
                    program.reportSyntaxErrors();
                } else {
                    program.execute(sourceFile);
                }
                break;
            default:
                throw new IllegalStateException("Unreachable code.");
        }
    }

    /**
     * Run the given Vikari source code string through the given phase with the given options.
     *
     * @param sourceString The source code string to interpret.
     * @param phase The phase of the interpreter to run the source file through.
     * @param lexerOptions An optional set of options for configuring output of the Lexer.
     */
    public static void runSourceString(String sourceString, Phase phase, LexerOptions lexerOptions,
                                       ParserOptions parserOptions) {
        log.debug("Run source string.");
        VikariProgram program = new VikariProgram();
        program.setLexerOptions(lexerOptions);
        program.setParserOptions(parserOptions);

        switch (phase) {
            case LEX:
                program.lex(sourceString);
                program.reportSyntaxErrors();
                break;
            case PARSE:
                program.lexAndParse(sourceString);
                program.reportSyntaxErrors();
                break;
            case EXECUTE:
            case DEFAULT:
                List<Statement> statements = program.lexAndParse(sourceString);
                if (program.hasErrors()) {
                    program.reportSyntaxErrors();
                } else {
                    program.execute(statements);
                }
                break;
            default:
                throw new IllegalStateException("Unreachable code.");
        }
    }

    /**
     * Run the interpreter in repl mode.
     * @param phase The phase of the interpreter to process code statements though.
     * @param lexerOptions Options to pass to the lexer.
     */
    public static void runReplMode(Phase phase, LexerOptions lexerOptions) {
        log.debug("Run repl mode.");
        log.debug("Not yet implemented.");
        // TODO: implement.
    }

    public static void printHelp(Options options) {
        log.trace("printHelp()");
        HelpFormatter helpFormatter = new HelpFormatter();
        String header = "\nInterpret the Vikari programming language.\n";
        String footer = "\n<config_options>: [litv] for each line of code in output:\n" +
         " l: [lines] Print line numbers before each line.\n" +
         " i: [invisibles] Show SPACE, TAB, and NEWLINE as `·`, `→`, and `¶`.\n" +
         " t: [tokens] Show each token as quoted strings separated by commas.\n" +
         " v: [verbose] Print each token's type name.\n" +
         "\n" +
         "<log_level>: one of the following:\n" +
         " ALL, TRACE, DEBUG, INFO, WARN, ERROR, FATAL, OFF.";
        helpFormatter.printHelp("vikari", header, options, footer, true);
    }

    public static void printVersion() {
        log.trace("printVersion()");
        try {
            final Properties properties = new Properties();
            properties.load(Main.class.getClassLoader().getResourceAsStream("vikari.properties"));
            String version = properties.getProperty("version");
            System.out.println(version);
        } catch (IOException e) {
            log.error(e);
            System.err.println("Error fetching version number from ``vikari.properties`` file.");
            System.exit(-1);
        }
    }

    public static void setLogLevel(Level logLevel) {
        LoggerContext loggerContext = (LoggerContext) LogManager.getContext(false);
        Configuration configuration = loggerContext.getConfiguration();
        String rootPackageName = Main.class.getPackage().getName();
        LoggerConfig loggerConfig = configuration.getLoggerConfig(rootPackageName);
        loggerConfig.setLevel(logLevel);
        loggerContext.updateLoggers();
    }
}
