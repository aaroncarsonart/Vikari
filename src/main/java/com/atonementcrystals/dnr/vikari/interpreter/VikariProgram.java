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
import java.util.Formatter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Holds all state for lexing, parsing, and interpreting a Vikari program.
 */
public class VikariProgram {
    private static final Logger log = LogManager.getLogger(VikariProgram.class);

    private final Lexer lexer;
    private final Parser parser;
    private final TreeWalkInterpreter interpreter;

    private LexerOptions lexerOptions;
    private ParserOptions parserOptions;

    private final Map<String, List<List<AtonementCrystal>>> lexerResults;
    private final Map<String, List<Statement>> parserResults;
    private final SyntaxErrorReporter syntaxErrorReporter;

    private final AtonementField globalAtonementField;

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

        globalAtonementField = initGlobalAtonementField();

        parser.setGlobalAtonementField(globalAtonementField);
        interpreter.setGlobalAtonementField(globalAtonementField);
    }

    /**
     * Sets up the global, top-level AtonementField which is shared for all files in the Parser and TreeWalkInterpreter.
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
            printMessageWithLines("Lex: \"" + filePath + "\"");
            printLexedStatements(lexedStatements, lexerOptions.statementNumbers, lexerOptions.showInvisibles,
                    lexerOptions.separateTokens, lexerOptions.verbose);
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
        log.info("Source code:\n{}", sourceCode);
        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceCode);

        if (shouldPrintLexerResults()) {
            printMessageWithLines("Lex:");
            printLexedStatements(lexedStatements, lexerOptions.statementNumbers, lexerOptions.showInvisibles,
                    lexerOptions.separateTokens, lexerOptions.verbose);
        }

        return lexedStatements;
    }

    /**
     * For executing the PARSE phase of the interpreter.
     * @param sourceFile The source file to lex and parse.
     */
    public void lexAndParse(File sourceFile) {
        lex(sourceFile);
        parse(sourceFile);
    }

    /**
     * Parse the output of the Lexer.
     * @param sourceFile The previously lexed Vikari source file to parse.
     */
    public List<Statement> parse(File sourceFile) {
        String filePath = sourceFile.getAbsolutePath();
        log.info("parse(\"{}\")", filePath);

        List<List<AtonementCrystal>> lexedStatements = lexerResults.get(filePath);
        List<Statement> parsedStatements = parser.parse(sourceFile, lexedStatements);
        parserResults.put(filePath, parsedStatements);

        if (shouldPrintParserResults()) {
            printMessageWithLines("Parse: \"" + filePath + "\"");
            printParsedStatements(parsedStatements, parserOptions.statementNumbers, parserOptions.verbose);
        }

        return parsedStatements;
    }

    /**
     * Parse the output of the Lexer.
     * @param lexedStatements The output of the Lexer to parse.
     */
    public List<Statement> parse(List<List<AtonementCrystal>> lexedStatements) {
        log.info("parse()");
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        if (shouldPrintParserResults()) {
            printMessageWithLines("Parse:");
            printParsedStatements(parsedStatements, parserOptions.statementNumbers, parserOptions.verbose);
        }

        return parsedStatements;
    }

    /**
     * For executing the PARSE phase of the interpreter.
     * @param sourceString The Vikari source code string to lex and parse.
     */
    public List<Statement> lexAndParse(String sourceString) {
        List<List<AtonementCrystal>> lexedStatements = lex(sourceString);
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

        if (shouldPrintExecuteBannerMessage()) {
            printMessageWithLines("Execute: \"" + filePath + "\"");
        } else if (shouldPrintProgramOutputBannerMessage()) {
            printMessageWithLines("Program Output:");
        }

        List<Statement> parsedStatements = parserResults.get(filePath);
        interpreter.interpret(sourceFile, parsedStatements);
    }

    /**
     * Execute the output of the Parser.
     * @param parsedStatements The output of the Parser to execute.
     */
    public void execute(List<Statement> parsedStatements) {
        log.info("execute()");

        if (shouldPrintExecuteBannerMessage()) {
            printMessageWithLines("Execute:");
        } else if (shouldPrintProgramOutputBannerMessage()) {
            printMessageWithLines("Program Output:");
        }

        interpreter.interpret(null, parsedStatements);
    }

    /**
     * Prints the output of the Lexer based on the input formatting flags. By default, this method just prints
     * out each token's identifier, separated by spaces, without any additional formatting applied.<br/>
     * <br/>
     * Tokens that are elided by the Lexer, including statement separators, line continuations, and comments,
     * are not printed by this method.
     *
     * @param lexedStatements The list of lexed statements to print.
     * @param statementNumbers Prints the current statement number before each lexed statement.
     * @param showInvisibles Replaces spaces, tabs, and newlines in identifiers with "·", "→", and "¶".
     * @param separateTokens Prints each token in quotes, separated by commas.
     * @param verbose Same as separateTokens, but also encloses each token with a label of its type name.
     */
    private void printLexedStatements(List<List<AtonementCrystal>> lexedStatements, boolean statementNumbers,
                                     boolean showInvisibles, boolean separateTokens, boolean verbose) {
        StringBuilder sb = new StringBuilder();
        Formatter formatter = new Formatter(sb);

        int maxStatementNumberCharWidth = String.valueOf(lexedStatements.size()).length();
        String statementNumberFormat = "[%0"+ maxStatementNumberCharWidth + "d]: ";

        for (int statementNumber = 0; statementNumber < lexedStatements.size(); statementNumber++) {
            if (statementNumbers) {
                formatter.format(statementNumberFormat, statementNumber + 1);
            }

            List<AtonementCrystal> statement = lexedStatements.get(statementNumber);
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
                        sb.append(", ");
                    }
                } else {
                    sb.append(identifier);
                    sb.append(' ');
                }

            }
            if (statementNumber < lexedStatements.size() - 1) {
                sb.append('\n');
            }
        }

        String result = sb.toString();
        log.trace("Lexed statements:\n{}", result);
        System.out.println(result);
    }

    /**
     * Prints the output of the Parser based in the input formatting flags. By default, this method wraps the
     * contents of each statement, and each expression containing more than one element, in square brackets
     * delimited by commas.
     *
     * @param parsedStatements The list of parsed statements to print.
     * @param statementNumbers Prints the current statement number before each parsed statement.
     * @param verbose Prepends a shorthand label describing the type of each statement, expression, or crystal.
     */
    private void printParsedStatements(List<Statement> parsedStatements, boolean statementNumbers, boolean verbose) {
        AstPrintVisitor astPrintVisitor = new AstPrintVisitor();
        astPrintVisitor.setVerbose(verbose);

        StringBuilder sb = new StringBuilder();
        Formatter formatter = new Formatter(sb);

        int maxStatementNumber = parsedStatements.size();
        int maxStatementNumberCharWidth = String.valueOf(maxStatementNumber).length();
        String statementNumberFormat = "[%0"+ maxStatementNumberCharWidth + "d]: ";

        int statementNumber = 0;
        for (Statement statement : parsedStatements) {
            if (statementNumbers) {
                formatter.format(statementNumberFormat, statementNumber + 1);
            }
            String result = statement.accept(astPrintVisitor);
            sb.append(result);
            if (statementNumber < parsedStatements.size() - 1) {
                sb.append('\n');
            }
            statementNumber++;
        }

        String result = sb.toString();
        log.trace("Parsed statements:\n{}", result);
        System.out.println(result);
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

    private boolean shouldPrintLexerResults() {
        Level logLevel = log.getLevel();
        return lexerOptions != null && lexerOptions.printTokens && (logLevel == Level.TRACE || logLevel == Level.DEBUG ||
                logLevel == Level.ALL);
    }

    private boolean shouldPrintParserResults() {
        Level logLevel = log.getLevel();
        return parserOptions != null && parserOptions.printAst && (logLevel == Level.TRACE || logLevel == Level.DEBUG ||
                logLevel == Level.ALL);
    }

    private boolean shouldPrintExecuteBannerMessage() {
        return shouldPrintLexerResults() || shouldPrintParserResults();
    }

    private boolean shouldPrintProgramOutputBannerMessage() {
        return hasWarnings();
    }

    private void printMessageWithLines(String message) {
        Utils.printLineOfChars('-', message.length());
        System.out.println(message);
        Utils.printLineOfChars('-', message.length());
    }
}
