package com.atonement.crystals.dnr.vikari;

import com.atonement.crystals.dnr.vikari.error.Vikari_TypeError;
import com.atonement.crystals.dnr.vikari.interpreter.LexerOptions;
import com.atonement.crystals.dnr.vikari.interpreter.Phase;
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

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Properties;

/**
 * Lex, parse, and interpret Vikari programs based on input of command-line arguments.
 */
public class Main {

    /**
     * Entry point of the program.
     * @param args All command-line arguments.
     */
    public static void main(String[] args) {
        runVikariProgram(args);
    }

    /**
     * Parse command-line arguments, and run the Vikari interpreter.
     * @param args All command-line arguments to parse.
     */
    public static void runVikariProgram(String[] args) {
        CommandLineParser commandLineParser = new DefaultParser();
        Options options = buildOptions();

        try {
            CommandLine cmd = commandLineParser.parse(options, args);
            boolean verbose = false;
            Phase phase = Phase.DEFAULT;
            LexerOptions lexerOptions = parseLexerOptions("lit");
            List<String> argsList = cmd.getArgList();

            // ------------------------------------------------------------------
            // 1. Run default behavior of interpreter with no optional arguments.
            // ------------------------------------------------------------------
            if (cmd.getOptions().length == 0) {
                System.out.println("No options provided.");

                // Run the only argument as a file.
                if (argsList.size() == 1) {
//                    System.out.println("One argument. Default behavior.");
//                    System.out.println("Use default phase.");
//                    System.out.println("Use default config:\n    " + lexerOptions);
//                    System.out.println("Use file OR type source option.");

                    String pathToSourceFile = argsList.get(0);

                    // Attempt to resolve first argument as a fully-qualified type name.
                    if (Utils.validateFullyQualifiedTypeName(pathToSourceFile)) {
                        pathToSourceFile = Utils.filePathForTypeName(pathToSourceFile);
                    }

                    VikariSourceFileLoader sourceFileLoader = new VikariSourceFileLoader();
                    File sourceFile = sourceFileLoader.loadSourceFile(pathToSourceFile);
                    runSourceFile(sourceFile, phase, lexerOptions, verbose);
                    return;
                }

                // Run repl mode.
                else if (argsList.size() == 0) {
//                    System.out.println("Zero arguments. Default behavior.");
//                    System.out.println("Use default phase.");
//                    System.out.println("Use default config:\n    " + lexerOptions);
//                    System.out.println("Use repl source option.");

                    runReplMode(phase, lexerOptions, verbose);
                    return;
                }
            }

            // -----------------------------------------------------
            // If too many arguments, print help message. Then exit.
            // -----------------------------------------------------
            if (argsList.size() > 1) {
                System.out.println("Too many arguments.");
                printHelp(options);
                return;
            }

            // -----------------------------------------------------
            // 2. Exit early if version or help option is requested.
            // -----------------------------------------------------
            if (cmd.hasOption("version")) {
                printVersion();
                return;
            }

            if (cmd.hasOption("help")) {
                printHelp(options);
                return;
            }

            // -------------------------
            // 3. Enable verbose output.
            // -------------------------
            if (cmd.hasOption("debug")) {
                verbose = true;
            }

            // -----------------------------------------
            // 4. Determine phase of interpreter to run.
            // -----------------------------------------
            // Lexer phase.
            if (cmd.hasOption("lex")) {
                if (verbose) System.out.println("Lexer phase selected.");
                if (verbose) System.out.println("Use default config:\n    " + lexerOptions);
                phase = Phase.LEX;
            } else if (cmd.hasOption("Lex")) {
                if (verbose) System.out.println("Lexer phase selected.");
                phase = Phase.LEX;

                String configArgument = cmd.getOptionValue("Lex");
                lexerOptions = parseLexerOptions(configArgument);
                if (verbose) System.out.println("Use config:\n    " + lexerOptions);
            }

            // Parser phase.
            else if (cmd.hasOption("parse")) {
                if (verbose) System.out.println("Parser phase selected.");
                if (verbose) System.out.println("Use default config:\n    " + lexerOptions);
                phase = Phase.PARSE;
            } else if (cmd.hasOption("Parse")) {
                if (verbose) System.out.println("Parser phase selected.");
                phase = Phase.PARSE;

                String configArgument = cmd.getOptionValue("Parse");
                lexerOptions = parseLexerOptions(configArgument);
                if (verbose) System.out.println("Use config:\n    " + lexerOptions);
            }

            // Execute phase.
            else if (cmd.hasOption("execute")) {
                if (verbose) {
                    System.out.println("Execute phase selected.");
                    System.out.println("Use default config:\n    " + lexerOptions);
                }
                phase = Phase.EXECUTE;
            } else if (cmd.hasOption("Execute")) {
                if (verbose) System.out.println("Execute phase selected.");
                phase = Phase.EXECUTE;

                String configArgument = cmd.getOptionValue("Execute");
                lexerOptions = parseLexerOptions(configArgument);
                if (verbose) System.out.println("Use config:\n    " + lexerOptions);
            }

            // No phase requested. Use default behavior.
            else {
                if (verbose) {
                    System.out.println("No phase selected.");
                    System.out.println("Use default phase.");
                    System.out.println("Use default config:\n    " + lexerOptions);
                }
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
                if (verbose) System.out.println("File source option selected.");
                pathToFile = cmd.getOptionValue("file");
                if (verbose) System.out.println("File: ``" + pathToFile +"``");
                expectedAdditionalArguments = 0;
            }

            // Get path to file from type or script name in option argument.
            else if (cmd.hasOption("type")) {
                if (verbose) System.out.println("Type source option selected.");
                String fullyQualifiedTypeName = cmd.getOptionValue("type");
                boolean isValid = Utils.validateFullyQualifiedTypeName(fullyQualifiedTypeName);
                if (isValid) {
                    pathToFile = Utils.filePathForTypeName(fullyQualifiedTypeName);
                    if (verbose) System.out.println("File: ``" + pathToFile + "``");
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
                if (verbose) System.out.println("Code source option selected.");
                sourceString = cmd.getOptionValue("code");
                if (verbose) System.out.println("Source code string: " + sourceString);
                expectedAdditionalArguments = 0;
            }

            // Run the repl mode for the interpreter.
            else if (cmd.hasOption("repl")) {
                if (verbose) System.out.println("Repl source option selected.");
                replMode = true;
                expectedAdditionalArguments = 0;
            }

            // If no source option selected, use the final argument as a file OR type name.
            // (If present.)
            else {
                if (verbose) System.out.println("No source option selected.");
                if (argsList.size() == 0) {
                    if (verbose) System.out.println("Using repl mode.");
                    replMode = true;
                } else if (argsList.size() == 1) {
                    if (verbose) System.out.println("Using file OR type mode.");
                    pathToFile = argsList.get(0);
                }
            }

            // If too many arguments, print help message. Then exit.
            if (argsList.size() > expectedAdditionalArguments) {
                System.out.println("Too many arguments.");
                printHelp(options);
                return;
            }

            // --------------------------------------
            // 6. Lex, parse, or execute the program.
            // --------------------------------------
            if (pathToFile != null) {
                if (verbose) System.out.println("Run source file.");
                VikariSourceFileLoader sourceFileLoader = new VikariSourceFileLoader();
                File sourceFile = sourceFileLoader.loadSourceFile(pathToFile);
                runSourceFile(sourceFile, phase, lexerOptions, verbose);
            } else if (sourceString != null) {
                if (verbose) System.out.println("Run source string.");
                runSourceString(sourceString, phase, lexerOptions, verbose);
            } else if (replMode) {
                runReplMode(phase, lexerOptions, verbose);
            }else {
//                runReplMode(phase, lexerOptions);
                throw new IllegalStateException("Unreachable code.");
            }

        } catch (ParseException e) {
            System.out.println(e.getMessage());
            printHelp(options);
        } catch (Vikari_TypeError e) {
            System.out.println(e.getMessage());
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

        Option debugOption = Option.builder("d").longOpt("debug")
                .desc("print verbose output")
                .build();

        Option helpOption = Option.builder("h").longOpt("help")
                .desc("show help dialog")
                .build();

        Options options = new Options();
        options.addOptionGroup(interpreterPhaseOptions);
        options.addOptionGroup(sourceLocationOptions);
        options.addOption(versionNumberOption);
        options.addOption(debugOption);
        options.addOption(helpOption);

        return options;
    }

    /**
     * Parse the LexerOptions from the {@literal <config_options>} argument.
     * @param optionsArgument The optional argument to parse.
     * @return The LexerOptions represented by the optional argument.
     */
    public static LexerOptions parseLexerOptions(String optionsArgument) {
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
     * Run the given Vikari source file through the given phase with the given options.
     *
     * @param sourceFile The source file to interpret.
     * @param phase The phase of the interpreter to run the source file through.
     * @param lexerOptions An optional set of options for configuring output of the Lexer.
     * @param verbose If true, then each phase of the interpreter prints a more verbose
     *                output of information regarding the processing of that phase.
     */
    public static void runSourceFile(File sourceFile, Phase phase, LexerOptions lexerOptions,
                                     boolean verbose) {
        VikariProgram program = new VikariProgram();
        program.setLexerOptions(lexerOptions);
        program.setPrintVerboseOutput(verbose);

        switch (phase) {
            case LEX:
                program.lex(sourceFile);
                break;
            case PARSE:
                program.lexAndParse(sourceFile);
                break;
            case EXECUTE:
            case DEFAULT:
                program.lexAndParse(sourceFile);
                program.execute();
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
                                       boolean verbose) {
        VikariProgram program = new VikariProgram();
        program.setLexerOptions(lexerOptions);
        program.setPrintVerboseOutput(verbose);

        switch (phase) {
            case LEX:
                program.lex(sourceString);
                break;
            case PARSE:
                program.lexAndParse(sourceString);
                break;
            case EXECUTE:
            case DEFAULT:
                program.lexAndParse(sourceString);
                program.execute();
                break;
            default:
                throw new IllegalStateException("Unreachable code.");
        }
    }

    /**
     * Run the interpreter in repl mode.
     * @param phase The phase of the interpreter to process code statements though.
     * @param lexerOptions Options to pass to the lexer.
     * @param verbose Show verbose output of the lexer, parser, and interpreter.
     */
    public static void runReplMode(Phase phase, LexerOptions lexerOptions, boolean verbose) {
        System.out.println("Run repl mode.");
        System.out.println("Not yet implemented.");
        // TODO: implement.
    }

    public static void printHelp(Options options) {
        HelpFormatter helpFormatter = new HelpFormatter();
        String header = "\nInterpret the Vikari programming language.\n";
        String footer = "\n<config_options>: [litv] for each line of code in output:\n" +
         " l: [lines] Print line numbers before each line.\n" +
         " i: [invisibles] Show SPACE, TAB, and NEWLINE as `·`, `→`, and `¶`.\n" +
         " t: [tokens] Show each token as quoted strings separated by commas.\n" +
         " v: [verbose] Print each token's type name.";
        helpFormatter.printHelp("vikari", header, options, footer, true);
    }

    public static void printVersion() {
        try {
            final Properties properties = new Properties();
            properties.load(Main.class.getClassLoader().getResourceAsStream("vikari.properties"));
            String version = properties.getProperty("version");
            System.out.println(version);
        } catch (IOException e) {
            System.err.println("Error fetching version number from ``vikari.properties`` file.");
            System.exit(-1);
        }
    }
}
