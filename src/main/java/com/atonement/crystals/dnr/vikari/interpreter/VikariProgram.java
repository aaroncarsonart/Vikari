package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.util.Utils;

import java.io.File;
import java.util.List;

/**
 * Holds all state for lexing, parsing, and interpreting a Vikari program.
 */
public class VikariProgram {
    private Lexer lexer;
    private Parser parser;
    private TreeWalkInterpreter interpreter;

    public VikariProgram() {
        lexer = new Lexer();
        parser = new Parser();
        interpreter = new TreeWalkInterpreter();
    }

    public List<List<AtonementCrystal>> lex(File sourceFile) {
        return lexer.lexVikariSourceFile(sourceFile);
    }

    public List<List<AtonementCrystal>> lex(String sourceCode) {
        return lexer.lexVikariSourceCode(sourceCode);
    }

    public void lexAndParse(File sourceFile) {
        List<List<AtonementCrystal>> lexedStatements = lex(sourceFile);

        boolean printLineNumbers = true;
        boolean showInvisibles = true;
        boolean separateTokens = true;
        boolean verbose = true;

        printLexedStatements(lexedStatements, printLineNumbers, showInvisibles, separateTokens, verbose);

        // TODO: Implement parser.
    }

    public void execute() {
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
