package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.util.Utils;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.Formatter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Holds all state for lexing, parsing, and interpreting a Vikari program.
 */
public class VikariProgram {
    private static final Logger log = LogManager.getLogger(VikariProgram.class);

    private Lexer lexer;
    private Parser parser;
    private TreeWalkInterpreter interpreter;

    private LexerOptions lexerOptions;

    private Map<String, List<List<AtonementCrystal>>> lexerResults;

    public VikariProgram() {
        log.trace("VikariProgram constructor.");
        lexer = new Lexer();
        parser = new Parser();
        interpreter = new TreeWalkInterpreter();
        lexerResults = new LinkedHashMap<>();
    }

    public void setLexerOptions(LexerOptions lexerOptions) {
        log.trace("setLexerOptions()");
        this.lexerOptions = lexerOptions;
    }


    /**
     * Execute the LEX phase of the interpreter. For lexing the contents of a Vikari source file.
     *
     * @param sourceFile The source file to lex.
     * @return The list of lexed statements.
     */
    public List<List<AtonementCrystal>> lex(File sourceFile) {
        String filePath = sourceFile.getAbsolutePath();
        log.info("lex(\"{}\")", filePath);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceFile(sourceFile);
        lexerResults.put(filePath, lexedStatements);

        Level logLevel = log.getLevel();
        if (lexerOptions != null && (logLevel == Level.TRACE) || logLevel == Level.DEBUG || logLevel == Level.ALL) {
            String message = "Lex file: \"" + filePath + "\"";

            Utils.printLineOfChars('-', message.length());
            System.out.println(message);
            Utils.printLineOfChars('-', message.length());

            printLexedStatements(lexedStatements,
                    lexerOptions.printLineNumbers,
                    lexerOptions.showInvisibles,
                    lexerOptions.separateTokens,
                    lexerOptions.verbose);
        }

        return lexedStatements;
    }

    /**
     * Execute the LEX phase of the interpreter. For lexing Vikari code statements directly.
     * (Such as from -c or --code {@literal <code>})
     *
     * @param sourceCode The source code to lex.
     * @return The list of lexed statements.
     */
    public List<List<AtonementCrystal>> lex(String sourceCode) {
        log.info("lex()");
        log.info("Source code: {}", sourceCode );

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceCode);

        Level logLevel = log.getLevel();
        if (lexerOptions != null && (logLevel == Level.TRACE) || logLevel == Level.DEBUG || logLevel == Level.ALL) {
            String message = "Lex code string.";

            Utils.printLineOfChars('-', message.length());
            System.out.println(message);
            Utils.printLineOfChars('-', message.length());

            printLexedStatements(lexedStatements,
                    lexerOptions.printLineNumbers,
                    lexerOptions.showInvisibles,
                    lexerOptions.separateTokens,
                    lexerOptions.verbose);
        }

        return lexedStatements;
    }

    /**
     * For executing the PARSE phase of the interpreter.
     * @param sourceFile The source file to lex and parse.
     */
    public void lexAndParse(File sourceFile) {

        // -----------------------
        // 1. Lex the source file.
        // -----------------------
        lex(sourceFile);

        // -------------------------
        // 2. Parse the source file.
        // -------------------------
        parse(sourceFile);
    }

    /**
     * Parse the output of the lexer.
     * @param sourceFile The previously lexed Vikari source file to parse.
     */
    public void parse(File sourceFile) {
        String filePath = sourceFile.getAbsolutePath();

        log.info("parse(\"{}\")", filePath);
        log.info("Parser phase is not implemented.");

        List<List<AtonementCrystal>> lexedStatements = lexerResults.get(filePath);

        // TODO: Implement parser.
    }

    /**
     * Parse the output of the lexer.
     * @param lexedStatements The output of the lexer to parse.
     */
    public void parse(List<List<AtonementCrystal>> lexedStatements) {
        log.info("parse()");
        log.info("Parser phase is not implemented.");

        // TODO: Implement parser.
    }


    /**
     * For executing the PARSE phase of the interpreter.
     * @param sourceString The Vikari source code string to lex and parse.
     */
    public void lexAndParse(String sourceString) {
        // -----------------------
        // 1. Lex the source code.
        // -----------------------
        List<List<AtonementCrystal>> lexedStatements = lex(sourceString);

        // -------------------------
        // 2. Parse the source code.
        // -------------------------
        parse(lexedStatements);
    }

    /**
     * Execute the given Vikari source file.
     * @param sourceFile The previously parsed Vikari source file to execute.
     */
    public void execute(File sourceFile) {
        String filePath = sourceFile.getAbsolutePath();

        log.info("execute(\"{}\")", filePath);
        log.info("Execute phase is not implemented.");

        // -----------------------------
        // 3. Execute the resulting AST.
        // -----------------------------
        // TODO: Implement interpreter.
    }

    /**
     * Execute the previously parsed Vikari source code string.
     */
    public void execute() {
        log.info("execute()");
        log.info("Execute phase is not implemented.");

        // -----------------------------
        // 2. Execute the resulting AST.
        // -----------------------------
        // TODO: Implement interpreter.
    }

    /**
     * Prints the output of the Lexer based on the input formatting flags.
     * By default, this method just prints out each token's identifier
     * without any additional formatting applied.<br/>
     * <br/>
     * <i>(The default output should exactly match the original input source file.)</i>
     *
     * @param lexedStatements The list of lexed statements to print.
     * @param printLineNumbers Prints the current line number before each lexed statment.
     * @param showInvisibles Replaces spaces, tabs, and newlines with "·", "→", and "¶".
     * @param separateTokens Prints each token in quotes, separated by commas.
     * @param verbose Prints each token with quotes enclosed by its type name.
     */
    public void printLexedStatements(List<List<AtonementCrystal>> lexedStatements, boolean printLineNumbers,
                                     boolean showInvisibles, boolean separateTokens, boolean verbose) {
        StringBuilder sb = new StringBuilder();
        Formatter formatter = new Formatter(sb);

        int maxLineNumberCharWidth = String.valueOf(lexedStatements.size()).length();
        String lineNumberFormat = "[line:%0"+ maxLineNumberCharWidth + "d] ";

        for (int lineNumber = 0; lineNumber < lexedStatements.size(); lineNumber++) {
            if (printLineNumbers) {
                formatter.format(lineNumberFormat, lineNumber);
            }

            List<AtonementCrystal> statement = lexedStatements.get(lineNumber);
            for (int i = 0; i < statement.size(); i++) {
                AtonementCrystal crystal = statement.get(i);

                String identifier = crystal.getIdentifier();
                if (showInvisibles) {
                    identifier = Utils.showInvisibles(identifier);
                }

                if (separateTokens || verbose) {
                    if (verbose) {
                        String typeName = Utils.getSimpleClassName(crystal);
                        formatter.format("%s(\"%s\")", typeName, identifier);
                    } else {
                        formatter.format("\"%s\"", identifier);
                    }
                    if ( i < statement.size() - 1) {
                        sb.append(",");
                    }
                } else {
                    sb.append(identifier);
                }

            }
            if (lineNumber < lexedStatements.size() - 1) {
                sb.append('\n');
            }
        }

        String result = sb.toString();
        log.trace("Lexed statements:\n{}", result);
        System.out.println(result);
    }
}
