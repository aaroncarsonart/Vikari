package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.util.Utils;

import java.io.File;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Holds all state for lexing, parsing, and interpreting a Vikari program.
 */
public class VikariProgram {
    private Lexer lexer;
    private Parser parser;
    private TreeWalkInterpreter interpreter;

    private LexerOptions lexerOptions;
    public boolean printVerboseOutput;

    public VikariProgram() {
        lexer = new Lexer();
        parser = new Parser();
        interpreter = new TreeWalkInterpreter();
    }

    public void setLexerOptions(LexerOptions lexerOptions) {
        this.lexerOptions = lexerOptions;
    }

    public void setPrintVerboseOutput(boolean printVerboseOutput) {
        this.printVerboseOutput = printVerboseOutput;
    }

    /**
     * Execute the LEX phase of the interpreter. For lexing the contents of a Vikari source file.
     *
     * @param sourceFile The source file to lex.
     * @return The list of lexed statements.
     */
    public List<List<AtonementCrystal>> lex(File sourceFile) {
        String filePath = sourceFile.getAbsolutePath();

        if (printVerboseOutput) {
            System.out.println("Lex source file: ``" + filePath + "``");
        }

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceFile(sourceFile);

        if (lexerOptions != null) {
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
        if (printVerboseOutput) {
            System.out.println("Lex source code: " + sourceCode );
        }

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceCode);

        if (lexerOptions != null) {
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
        List<List<AtonementCrystal>> lexedStatements = lex(sourceFile);

        // -------------------------
        // 2. Parse the source file.
        // -------------------------
        parse(lexedStatements);
    }

    /**
     * Parse the output of the lexer.
     * @param lexedStatements The output of the lexer to parse.
     */
    public void parse(List<List<AtonementCrystal>> lexedStatements) {
        if (printVerboseOutput) {
            System.out.println("Parse.");
            System.out.println("Parser phase is not implemented.");
        }

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
        if (printVerboseOutput) {
            System.out.println("Lex source code: " + sourceString);
        }

        List<List<AtonementCrystal>> lexedStatements = lex(sourceString);

        // -------------------------
        // 2. Parse the source code.
        // -------------------------
        if (printVerboseOutput) {
            System.out.println("Parse source code: " + sourceString);
            System.out.println("Parser phase is not implemented.");
        }

        parse(lexedStatements);
    }

    public void execute() {
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

        int maxLineNumberCharWidth = String.valueOf(lexedStatements.size()).length();
        String lineNumberFormat = "[line:%0"+ maxLineNumberCharWidth + "d] ";

        for (int lineNumber = 0; lineNumber < lexedStatements.size(); lineNumber++) {
            if (printLineNumbers) {
                System.out.printf(lineNumberFormat, lineNumber);
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
                        System.out.printf("%s(\"%s\")", typeName, identifier);
                    } else {
                        System.out.printf("\"%s\"", identifier);
                    }
                    if ( i < statement.size() - 1) {
                        System.out.print(",");
                    }
                } else {
                    System.out.print(identifier);
                }

            }
            System.out.println();
        }
    }
}
