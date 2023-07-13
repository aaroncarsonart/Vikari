package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.AstPrintVisitor;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.util.Utils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.util.ArrayList;
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
    private ParserOptions parserOptions;

    private Map<String, List<List<AtonementCrystal>>> lexerResults;
    private Map<String, List<Statement>> parserResults;
    private SyntaxErrorReporter syntaxErrorReporter;

    private List<List<List<AtonementCrystal>>> replLexerResults;
    private List<List<Statement>> replParserResults;

    private AtonementField globalAtonementField;

    public VikariProgram() {
        log.trace("VikariProgram constructor.");
        lexer = new Lexer();
        parser = new Parser();
        interpreter = new TreeWalkInterpreter();

        lexerResults = new LinkedHashMap<>();
        parserResults = new LinkedHashMap<>();
        syntaxErrorReporter = new SyntaxErrorReporter();

        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLineFromCache);

        replLexerResults = new ArrayList<>();
        replParserResults = new ArrayList<>();

        globalAtonementField = initGlobalAtonementField();

        parser.setGlobalAtonementField(globalAtonementField);
        interpreter.setGlobalAtonementField(globalAtonementField);
    }

    /**
     * Sets up the global, top-level AtonementField which is shared for all files
     * in the parser and interpreter.
     */
    public static AtonementField initGlobalAtonementField() {
        // Initialize with no parent and shadowing enabled.
        AtonementField globalAtonementField = new AtonementField();

        // Add all lang types to the global field.
        for (VikariType vikariType : VikariType.LANG_TYPES) {
            TypeCrystal typeCrystal = vikariType.getTypeCrystal();
            String typeName = typeCrystal.getTypeName();
            String fullyQualifiedTypeName = typeCrystal.getFullyQualifiedTypeName();

            globalAtonementField.define(typeName, typeCrystal);
            globalAtonementField.define(fullyQualifiedTypeName, typeCrystal);
        }
        return globalAtonementField;
    }

    public AtonementField getGlobalAtonementField() {
        return globalAtonementField;
    }

    public void setLexerOptions(LexerOptions lexerOptions) {
        log.trace("setLexerOptions()");
        this.lexerOptions = lexerOptions;
        if (lexerOptions != null) {
            lexer.setCompilationWarningsEnabled(lexerOptions.warnings);
        }
    }

    public void setParserOptions(ParserOptions parserOptions) {
        log.trace("setParserOptions()");
        this.parserOptions = parserOptions;
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

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceFile);
        lexerResults.put(filePath, lexedStatements);

        if (shouldPrintLexerResults()) {
            printMessageWithLines("Lex file: \"" + filePath + "\"");
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
        log.info("Source code:\n{}", sourceCode );

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceCode);
        replLexerResults.add(lexedStatements);

        if (shouldPrintLexerResults()) {
            printMessageWithLines("Lex code string.");
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
    public List<Statement> parse(File sourceFile) {
        String filePath = sourceFile.getAbsolutePath();
        log.info("parse(\"{}\")", filePath);

        List<List<AtonementCrystal>> lexedStatements = lexerResults.get(filePath);
        List<Statement> parsedStatements = parser.parse(sourceFile, lexedStatements);
        parserResults.put(filePath, parsedStatements);

        if (shouldPrintParserResults()) {
            printMessageWithLines("Parse file: \"" + filePath + "\"");
            printParsedStatements(parsedStatements,
                    parserOptions.printLineNumbers,
                    parserOptions.printAst,
                    parserOptions.verbose);
        }

        return parsedStatements;
    }

    /**
     * Parse the output of the lexer.
     * @param lexedStatements The output of the lexer to parse.
     */
    public List<Statement> parse(List<List<AtonementCrystal>> lexedStatements) {
        log.info("parse()");
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);
        replParserResults.add(parsedStatements);

        if (shouldPrintParserResults()) {
            printMessageWithLines("Parse code string.");
            printParsedStatements(parsedStatements,
                    parserOptions.printLineNumbers,
                    parserOptions.printAst,
                    parserOptions.verbose);
        }

        return parsedStatements;
    }

    /**
     * For executing the PARSE phase of the interpreter.
     * @param sourceString The Vikari source code string to lex and parse.
     */
    public List<Statement> lexAndParse(String sourceString) {
        // -----------------------
        // 1. Lex the source code.
        // -----------------------
        List<List<AtonementCrystal>> lexedStatements = lex(sourceString);

        // -------------------------
        // 2. Parse the source code.
        // -------------------------
        List<Statement> parsedStatements = parse(lexedStatements);
        return parsedStatements;
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
        List<Statement> parsedStatements = parserResults.get(filePath);
        interpreter.interpret(sourceFile, parsedStatements);
    }

    /**
     * Execute the previously parsed Vikari source code string.
     */
    public void execute(List<Statement> parsedStatements) {
        log.info("execute()");

        // -----------------------------
        // 3. Execute the resulting AST.
        // -----------------------------
        interpreter.interpret(null, parsedStatements);
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
    public static void printLexedStatements(List<List<AtonementCrystal>> lexedStatements, boolean printLineNumbers,
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

    public static void printParsedStatements(List<Statement> parsedStatements, boolean printAst,
                                             boolean printLineNumbers, boolean verbose) {
        if (printAst) {
            AstPrintVisitor astPrintVisitor = new AstPrintVisitor();
            astPrintVisitor.setVerbose(verbose);

            StringBuilder sb = new StringBuilder();
            Formatter formatter = new Formatter(sb);

            int maxLineNumber = parsedStatements.size() - 1;
            int maxLineNumberCharWidth = String.valueOf(maxLineNumber).length();
            String lineNumberFormat = "[line:%0"+ maxLineNumberCharWidth + "d] ";

            int lineNumber = 0;
            for (Statement statement : parsedStatements) {
                if (printLineNumbers) {

                    if (printLineNumbers) {
                        formatter.format(lineNumberFormat, lineNumber++);
                    }
                }
                String result = statement.accept(astPrintVisitor);
                sb.append(result);
                if (lineNumber < parsedStatements.size() - 1) {
                    sb.append('\n');
                }

            }

            String result = sb.toString();
            log.trace("Parsed statements:\n{}", result);
            System.out.println(result);
        }
    }

    public boolean hasErrors() {
        return syntaxErrorReporter.hasErrors();
    }

    public boolean hasWarnings() {
        return syntaxErrorReporter.hasWarnings();
    }

    public void reportErrors() {
        if (syntaxErrorReporter.hasErrors()) {
            syntaxErrorReporter.reportSyntaxErrors();
        }
    }

    public void reportWarnings() {
        if (syntaxErrorReporter.hasWarnings()) {
            syntaxErrorReporter.reportWarnings();
        }
    }

    public boolean shouldPrintLexerResults() {
        Level logLevel = log.getLevel();
        return lexerOptions != null && lexerOptions.printTokens && (logLevel == Level.TRACE || logLevel == Level.DEBUG ||
                logLevel == Level.ALL);
    }

    public boolean shouldPrintParserResults() {
        Level logLevel = log.getLevel();
        return parserOptions != null && parserOptions.printAst && (logLevel == Level.TRACE || logLevel == Level.DEBUG ||
                logLevel == Level.ALL);
    }

    private void printMessageWithLines(String message) {
        Utils.printLineOfChars('-', message.length());
        System.out.println(message);
        Utils.printLineOfChars('-', message.length());
    }
}
