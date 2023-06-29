package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.keyword.error.ThrowCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.MultiLineStringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.StringLiteralCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.SwordCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.control.flow.ContinueOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.AddOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.ModulusOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.MultiplyOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.math.SubtractOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix.DeleteOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.quotation.CaptureQuotationCrystal;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.error.Vikari_IOException;
import com.atonementcrystals.dnr.vikari.error.Vikari_LexerException;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import com.atonementcrystals.dnr.vikari.util.Utils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Performs the lexical analysis step of scanning the contents of a
 * Vikari source file or string and splitting it into string tokens.
 * These tokens are then converted to their associated crystal types
 * as the final output of the Lexer.
 */
public class Lexer {
    private static final Logger log = LogManager.getLogger(Lexer.class);
    private static final char END_OF_LINE = '\0';

    /** This pattern is intentionally missing the opening backtick to match after it has been consumed. */
    private static final String BACKTICK_CHARACTER_LITERAL_PATTERN = "\\``";
    private static final String INVALID_CHARACTERS_ERROR_MESSAGE = "Invalid characters.";
    private static final String DASH_CHARACTER_STRING = "-";

    private static final Pattern whitespaceRegex = Pattern.compile("^[ \t]+");
    private static final Pattern numberRegex = Pattern.compile("^\\d+(?:\\.\\d+)?(?i)[LFDB]?");
    private static final Pattern identifierRegex = Pattern.compile("^[A-Za-z_]\\w*");
    private static final Pattern invalidCharactersRegex = Pattern.compile("^[^\t -~]+");

    /**
     * If a negation operator preceding another negation operator follows one of these tokens,
     * the operators should be collapsed together into a single ThrowCrystal.
     */
    private static final HashSet<Class<? extends AtonementCrystal>> COLLAPSE_THROW_CRYSTAL_CLASSES = Stream.of(
                    TokenType.STATEMENT_SEPARATOR, TokenType.REGION_SEPARATOR, TokenType.REGION_OPERATOR)
            .map(TokenType::getJavaType)
            .collect(Collectors.toCollection(HashSet::new));

    /**
     * If a negation operator preceding a number literal follows one of these tokens,
     * the operator should be collapsed together with the number literal.
     */
    private static final HashSet<Class<? extends AtonementCrystal>> COLLAPSE_NEGATION_OPERATOR_CLASSES = Stream.of(
                    TokenType.RETURN, TokenType.BREAK, TokenType.CONTINUE, TokenType.LEFT_SQUARE_BRACKET,
                    TokenType.STATEMENT_SEPARATOR, TokenType.REGION_SEPARATOR, TokenType.LEFT_PARENTHESIS,
                    TokenType.LIST_ELEMENT_SEPARATOR, TokenType.RANGE, TokenType.TYPE_LABEL,
                    TokenType.INDEX_OPERATOR, TokenType.COPY_CONSTRUCTOR, TokenType.MODULUS, TokenType.MULTIPLY,
                    TokenType.SUBTRACT, TokenType.LEFT_ASSIGNMENT, TokenType.LEFT_ADD_ASSIGNMENT,
                    TokenType.LEFT_SUBTRACT_ASSIGNMENT, TokenType.LEFT_DIVIDE_ASSIGNMENT,
                    TokenType.LEFT_MULTIPLY_ASSIGNMENT, TokenType.LEFT_LOGICAL_AND_ASSIGNMENT,
                    TokenType.LEFT_LOGICAL_OR_ASSIGNMENT, TokenType.RIGHT_ADD_ASSIGNMENT,
                    TokenType.RIGHT_SUBTRACT_ASSIGNMENT, TokenType.RIGHT_DIVIDE_ASSIGNMENT,
                    TokenType.RIGHT_MULTIPLY_ASSIGNMENT, TokenType.RIGHT_LOGICAL_AND_ASSIGNMENT,
                    TokenType.RIGHT_LOGICAL_OR_ASSIGNMENT, TokenType.ADD, TokenType.LEFT_DIVIDE,
                    TokenType.RIGHT_DIVIDE, TokenType.LOGICAL_AND, TokenType.LOGICAL_OR,
                    TokenType.LOGICAL_NOT, TokenType.EQUALS, TokenType.REFERENCE_EQUALS, TokenType.LESS_THAN,
                    TokenType.GREATER_THAN, TokenType.GREATER_THAN_OR_EQUALS, TokenType.LESS_THAN_OR_EQUALS,
                    TokenType.KEY_VALUE_PAIR, TokenType.ITERATION_ELEMENT, TokenType.INSTANCE_OF,
                    TokenType.CATCH_ALL, TokenType.LEFT_FEATHER_FALL, TokenType.RIGHT_FEATHER_FALL)
            .map(TokenType::getJavaType)
            .collect(Collectors.toCollection(HashSet::new));

    private SyntaxErrorReporter syntaxErrorReporter;
    private File currentFile;
    private BufferedReader reader;

    private int lineNumber;
    private int startIndex;
    private int currentIndex;
    private String line;
    private int lineLength;
    private char nextChar;

    private List<String> lines;
    private List<String> stringTokens;
    private List<List<String>> statementsOfStringTokens;

    private int startLineNumber;

    public static HashSet<Class<? extends AtonementCrystal>> getCollapseNegationOperatorClasses() {
        return COLLAPSE_NEGATION_OPERATOR_CLASSES;
    }

    public void setSyntaxErrorReporter(SyntaxErrorReporter syntaxErrorReporter) {
        this.syntaxErrorReporter = syntaxErrorReporter;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    /**
     * Lexes a Vikari source file into a sequence of AtonementCrystals.
     *
     * @param sourceFile The Vikari source file to lex.
     * @return The sequence of AtonementCrystals defined by the Vikari source file.
     */
    public List<List<AtonementCrystal>> lex(File sourceFile) {
        log.trace("lex({})", sourceFile == null ? "null" : "\"" + sourceFile + "\"");
        currentFile = sourceFile;

        List<List<String>> statementsOfStringTokens = lexToStringTokens(sourceFile);
        List<List<AtonementCrystal>> statementsOfCrystals = convertTokensToCrystals(statementsOfStringTokens);

        currentFile = null;
        return statementsOfCrystals;
    }

    /**
     * Lexes a string of Vikari source code into a sequence of AtonementCrystals.
     *
     * @param sourceCode The string of Vikari source code to lex.
     * @return The sequence of AtonementCrystals defined by the string of Vikari source code.
     */
    public List<List<AtonementCrystal>> lex(String sourceCode) {
        log.trace("lex()");
        currentFile = null;
        List<List<String>> statementsOfStringTokens = lexToStringTokens(sourceCode);
        List<List<AtonementCrystal>> statementsOfCrystals = convertTokensToCrystals(statementsOfStringTokens);
        return statementsOfCrystals;
    }

    /**
     * Lexes a Vikari source file into a sequence of string tokens.
     *
     * @param sourceFile The Vikari source file to lex.
     * @return  A sequence of string tokens.
     */
    public List<List<String>> lexToStringTokens(File sourceFile) {
        try (BufferedReader reader = new BufferedReader(new FileReader(sourceFile))) {
            return readFromBufferAsStringTokens(reader);
        } catch (IOException e) {
            throw new Vikari_IOException("Error reading Vikari source file: " +
                    sourceFile.getName());
        }
    }


    /**
     * Lexes a string of Vikari source code into a sequence of string tokens.
     *
     * @param sourceString The string of Vikari source code to lex.
     * @return  A sequence of string tokens.
     */
    public List<List<String>> lexToStringTokens(String sourceString) {
        try (BufferedReader reader = new BufferedReader(new StringReader(sourceString))) {
            return readFromBufferAsStringTokens(reader);
        } catch (IOException e) {
            String newlineOrSpace = sourceString.contains("\n") ? "\n" : " ";
            throw new Vikari_IOException("Error reading Vikari source code:" + newlineOrSpace + sourceString);
        }
    }

    /**
     * Lexes the input read by the BufferedReader into a sequence of string tokens.
     *
     * @param reader The BufferedReader to read input from.
     * @return A sequence of string tokens.
     * @throws IOException If an IO error occurs.
     */
    private List<List<String>> readFromBufferAsStringTokens(BufferedReader reader) {
        this.reader = reader;
        statementsOfStringTokens = new ArrayList<>();
        stringTokens = new ArrayList<>();
        prepareLineDataCache();

        line = readNextLine();
        lineLength = atEndOfFile() ? 0 : line.length();
        startLineNumber = lineNumber;

        while (!atEndOfFile()) {

            while (startIndex < lineLength && !atEndOfFile()) {
                startIndex = currentIndex;
                advance();

                switch (nextChar) {
                    case '!':
                    case '#':
                    case '$':
                    case '%':
                    case '&':
                    case '\'':
                    case '(':
                    case ')':
                    case ',':
                    case ';':
                    case '@':
                    case '[':
                    case ']':
                    case '{':
                    case '}':
                        token();
                        break;

                    case '"':
                    case '*':
                        tryMatchAndGetToken("<<", ">>");
                        break;

                    case '+':
                        tryMatchAndGetToken("<<", ">>", "+");
                        break;

                    case '-':
                        tryMatchAndGetToken("<<", ">>", ">");
                        break;

                    case '.':
                        tryMatchAndGetToken("..", ".");
                        break;

                    case '/':
                        tryMatchAndGetToken("<<", "~/", "/");
                        break;

                    case ':':
                        tryMatchAndGetToken(":");
                        break;

                    case '<':
                        tryMatchAndGetToken("=>", "-", "<", "=", ">");
                        break;

                    case '=':
                        tryMatchAndGetToken(">");
                        break;

                    case '>':
                        tryMatchAndGetToken("=", ">");
                        break;

                    case '?':
                        tryMatchAndGetToken("<<", ">>", "?");
                        break;

                    case '\\':
                        tryMatchAndGetToken(">>", "\\");
                        break;

                    case '^':
                        tryMatchAndGetToken("<<", ">>", "^");
                        break;

                    case '`':
                        handleBacktick();
                        break;

                    case '|':
                        tryMatchAndGetToken("|");
                        break;

                    case '~':
                        if (match(":")) {
                            comment();
                        } else {
                            token();
                        }
                        break;

                    case ' ':
                    case '\t':
                        whitespaceToken();
                        break;

                    case END_OF_LINE:
                        startIndex = lineLength;
                        break;

                    default:
                        if (!(tryMatchIdentifierToken() || tryMatchNumberToken())) {
                            invalidCharactersToken();
                        }
                        break;
                }
            }

            if (!atEndOfFile()) {
                advanceToNextLine();
            }
        }
        return statementsOfStringTokens;
    }

    private void prepareLineDataCache() {
        if (syntaxErrorReporter == null) {
            syntaxErrorReporter = new SyntaxErrorReporter();
        }
        lines = syntaxErrorReporter.getLineCacheFor(currentFile);
    }

    public void resetTo(int lineNumber) {
        this.lineNumber = lineNumber;
        for (int i = lines.size() - 1; i >= lineNumber; i--) {
            lines.remove(i);
        }
    }

    private String readNextLine() {
        try {
            String line = reader.readLine();
            if (line != null) {
                lines.add(line);
            }
            return line;
        } catch (IOException e) {
            String messageSuffix;
            if (currentFile != null) {
                messageSuffix = " from Vikari source file: " + currentFile.getName();
            } else {
                messageSuffix = " of source code.";
            }
            throw new Vikari_IOException("IO error reading line " + lineNumber + messageSuffix);
        }
    }

    private boolean atEndOfFile() {
        return line == null;
    }

    private void advance() {
        if (currentIndex < lineLength) {
            nextChar = line.charAt(currentIndex++);
        } else {
            nextChar = END_OF_LINE;
        }
    }

    private void advanceToNextLine() {
        statementsOfStringTokens.add(stringTokens);
        stringTokens = new ArrayList<>();

        lineNumber++;
        startIndex = 0;
        currentIndex = 0;
        line = readNextLine();

        if (!atEndOfFile()) {
            lineLength = line.length();
        }
    }

    private void token() {
        String nextToken = line.substring(startIndex, currentIndex);
        stringTokens.add(nextToken);
    }

    private void tryMatchAndGetToken(String... matches) {
        tryMatch(matches);
        token();
    }

    /**
     * Try to match each string to the current position in the line.
     * Organize arguments so longer strings are matched first.
     * @param matches The strings to match against.
     */
    private void tryMatch(String... matches) {
        for (String text : matches) {
            if (match(text)) {
                currentIndex += text.length();
                break;
            }
        }
    }

    private boolean match(String text) {
        return line.regionMatches(currentIndex, text, 0, text.length());
    }

    private void handleBacktick() {
        if (match("`")) {
            stringToken();
        } else {
            backtickQuotedIdentifierToken();
        }
    }

    private void stringToken() {
        // Cache values in case of a syntax error.
        int errorLineNumber = lineNumber;
        int errorStart = startIndex;

        // Consume the string token(s).
        boolean consumedClosingToken = multilineToken(TokenType.CAPTURE_QUOTATION.getIdentifier());

        // Report if there was an error.
        if (!consumedClosingToken) {
            String errorMessage = "Missing closing capture quotation ``.";
            reportError(errorMessage, errorLineNumber, errorStart);
        }
    }

    private void backtickQuotedIdentifierToken() {
        // Handle an escaped backtick \` for a backtick character literal token.
        if (match(BACKTICK_CHARACTER_LITERAL_PATTERN)) {
            currentIndex += BACKTICK_CHARACTER_LITERAL_PATTERN.length();
            token();
            return;
        }

        advance();

        int closingBacktickIndex = line.indexOf('`', currentIndex);
        boolean missingClosingBacktick = false;
        if (closingBacktickIndex != -1) {
            currentIndex = closingBacktickIndex + 1;
        } else {
            currentIndex = line.length();
            reportError("Missing closing backtick quotation `.", lineNumber, startIndex);
            missingClosingBacktick = true;
        }

        token();

        // Check other error cases.
        if (!missingClosingBacktick) {
            String token = stringTokens.get(stringTokens.size() - 1);

            String backtick = TokenType.BACKTICK.getIdentifier();
            String unquotedNextToken = Utils.stripEnclosure(token, backtick, backtick);
            boolean isSpaceCharLiteral = unquotedNextToken.equals(" ");

            if (!isSpaceCharLiteral && Utils.isWhitespace(unquotedNextToken)) {
                // Report an error if identifier contains only whitespace.
                String errorMessage = "Backtick-quoted identifiers cannot contain only whitespace.";
                reportError(errorMessage, lineNumber, startIndex + 1);
            } else if (unquotedNextToken.contains("\t")) {
                // Report an error if identifier contains a tab character.
                String errorMessage = "Backtick-quoted identifiers cannot contain tabs.";
                reportError(errorMessage, lineNumber, startIndex + 1);
            }
        }
    }

    private void comment() {
        // Consume the comment token(s).
        Stack<CoordinatePair> unclosedOpeningTokens = multilineNestableCommentToken();

        // Report if there were any errors.
        for (int i = 0; i < unclosedOpeningTokens.size(); i++) {
            CoordinatePair location = unclosedOpeningTokens.get(i);
            reportError("Missing comment suffix token `:~`.", location);
        }
    }

    /**
     * Consumes characters until either the closing token or the end of file is reached.
     * @param closingToken The string which ends this multi-line token.
     * @return True if the closing token was reached, else false.
     */
    private boolean multilineToken(String closingToken) {
        advance();

        do {
            int indexOfClosingToken = line.indexOf(closingToken, currentIndex);

            if (indexOfClosingToken == -1) {
                currentIndex = lineLength;
                token();
                advanceToNextLine();
                continue;
            }

            currentIndex = indexOfClosingToken;
            if (!Utils.isEscapedByBackslash(line, indexOfClosingToken)) {
                currentIndex += closingToken.length();
                token();
                return true;
            } else if (currentIndex + closingToken.length() >= lineLength) {
                currentIndex = lineLength;
                token();
                advanceToNextLine();
            } else {
                currentIndex++;
            }

        } while (!atEndOfFile());

        // closingToken was not encountered. This is a syntax error.
        return false;
    }

    private Stack<CoordinatePair> multilineNestableCommentToken() {
        String openingToken = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        String closingToken = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();

        advance();

        Stack<CoordinatePair> unclosedOpeningTokens = new Stack<>();
        unclosedOpeningTokens.push(new CoordinatePair(lineNumber, startIndex));

        boolean firstToken = true;

        do {
            int nextOpeningTokenIndex = line.indexOf(openingToken, currentIndex);
            int nextClosingTokenIndex = line.indexOf(closingToken, currentIndex);
            int nextMatch = nextMultilineNestableTokenIndex(nextOpeningTokenIndex, nextClosingTokenIndex);

            // No tokens matched. Consume the rest of the line, and advance.
            if (nextMatch == -1) {
                currentIndex = lineLength;

                if (firstToken) {
                    openingCommentToken();
                    firstToken = false;
                } else {
                    middleCommentToken();
                }

                advanceToNextLine();
                continue;
            }

            currentIndex = nextMatch;
           if (nextOpeningTokenIndex == currentIndex) {
                currentIndex += openingToken.length();
                if (!Utils.isEscapedByBackslash(line, nextMatch)) {
                    unclosedOpeningTokens.push(new CoordinatePair(lineNumber, nextMatch));
                }
            } else {
                currentIndex += closingToken.length();
                if (!Utils.isEscapedByBackslash(line, nextMatch)) {
                    unclosedOpeningTokens.pop();
                }
            }

            // Reached end of all nestable tokens. Consume the final token, and end.
            if (unclosedOpeningTokens.isEmpty()) {
                if (firstToken) {
                    singleLineCommentToken();
                } else {
                    closingCommentToken();
                }
                break;
            }
        } while (!atEndOfFile());

        // Return all unclosed opening token locations for syntax error reporting purposes.
        return unclosedOpeningTokens;
    }

    private int nextMultilineNestableTokenIndex(int openingTokenIndex, int closingTokenIndex) {
        if (openingTokenIndex == -1 && closingTokenIndex == -1) return -1;
        else if (openingTokenIndex == -1) return closingTokenIndex;
        else if (closingTokenIndex == -1) return openingTokenIndex;
        return Math.min(openingTokenIndex, closingTokenIndex);
    }


    // Comments are nestable. All characters between the opening and
    // closing tokens are lexed as dashes so the nestable comment tokens
    // don't interfere with detecting the end of a multi-line comment.

    private void singleLineCommentToken() {
        String openingToken = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        String closingToken = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();

        int tokenLength = currentIndex - startIndex;
        int dashCount = tokenLength - openingToken.length() - closingToken.length();
        String dashes = DASH_CHARACTER_STRING.repeat(dashCount);

        String nextToken = openingToken + dashes + closingToken;
        stringTokens.add(nextToken);
    }

    private void openingCommentToken() {
        String openingToken = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();

        int tokenLength = currentIndex - startIndex;
        int dashCount = tokenLength - openingToken.length();
        String dashes = DASH_CHARACTER_STRING.repeat(dashCount);

        String nextToken = openingToken + dashes;
        stringTokens.add(nextToken);
    }

    private void middleCommentToken() {
        int tokenLength = currentIndex - startIndex;
        String nextToken = DASH_CHARACTER_STRING.repeat(tokenLength);
        stringTokens.add(nextToken);
    }

    private void closingCommentToken() {
        String closingToken = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();

        int tokenLength = currentIndex - startIndex;
        int dashCount = tokenLength - closingToken.length();
        String dashes = DASH_CHARACTER_STRING.repeat(dashCount);

        String nextToken = dashes + closingToken;
        stringTokens.add(nextToken);
    }

    private void whitespaceToken() {
        boolean consumedToken = tryMatchRegex(whitespaceRegex);
        if (!consumedToken) {
            throw new Vikari_LexerException("Internal error. Whitespace character '" + nextChar + "'" +
                    " not detected by regular expression.");
        }
    }

    private boolean tryMatchIdentifierToken() {
        return tryMatchRegex(identifierRegex);
    }

    private boolean tryMatchNumberToken() {
        return tryMatchRegex(numberRegex);
    }

    private void invalidCharactersToken() {
        boolean consumedToken = tryMatchRegex(invalidCharactersRegex);
        if (consumedToken) {
            reportError(INVALID_CHARACTERS_ERROR_MESSAGE, lineNumber, startIndex);
        } else {
            throw new Vikari_LexerException("Internal error. Invalid character '" + nextChar + "'" +
                    " not detected by regular expression.");
        }
    }

    private boolean tryMatchRegex(Pattern pattern) {
        Matcher matcher = pattern.matcher(line);
        matcher.region(startIndex, lineLength);

        if (matcher.find()) {
            currentIndex = matcher.end();
            token();

            return true;
        }

        return false;
    }

    /**
     * Report a SyntaxError.
     * @param message The error message.
     * @param row The row number location for the error.
     * @param column The column number location for the error.
     */
    private void reportError(String message, int row, int column) {
        CoordinatePair location = new CoordinatePair(row, column);
        reportError(message, location);
    }

    /**
     * Report a SyntaxError.
     * @param message The error message.
     * @param location The location of the error.
     */
    private void reportError(String message, CoordinatePair location) {
        SyntaxError syntaxError = new SyntaxError(currentFile, location, message);
        syntaxErrorReporter.add(syntaxError);
    }

    /**
     * Converts string tokens into their associated crystal types.
     * Identifiers which are references will be resolved later by the parser.
     *
     * @param statementsOfStringTokens The string tokens to convert.
     * @return The crystals representing each string token.
     */
    public List<List<AtonementCrystal>> convertTokensToCrystals(List<List<String>> statementsOfStringTokens) {
        List<List<AtonementCrystal>> statementsOfCrystals = new ArrayList<>();

        Map<String, TokenType> defaultIdentifiersMap = new LinkedHashMap<>();
        for (TokenType identifierMapping : TokenType.LEXER_TOKENS) {
            defaultIdentifiersMap.put(identifierMapping.getIdentifier(), identifierMapping);
        }

        for (int statementNumber = 0; statementNumber < statementsOfStringTokens.size(); statementNumber++) {
            List<String> statementOfStringTokens = statementsOfStringTokens.get(statementNumber);
            List<AtonementCrystal> statementOfCrystals = new ArrayList<>();
            int column = 0;

            for (int tokenNumber = 0; tokenNumber < statementOfStringTokens.size(); tokenNumber++) {
                String stringToken = statementOfStringTokens.get(tokenNumber);

                // Advance column number only once.
                if (tokenNumber > 0) {
                    String previousToken = statementOfStringTokens.get(tokenNumber - 1);
                    column += previousToken.length();
                }

                int row = startLineNumber + statementNumber;
                CoordinatePair tokenCoordinates = new CoordinatePair(row, column);

                if (Utils.isWhitespace(stringToken)) {
                    // All whitespace (besides indentation) is omitted in the final output of the Lexer.
                    // TODO: Produce IndentationCrystals for leading whitespace.
                    continue;
                }

                if (Utils.isBooleanLiteral(stringToken)) {
                    // TODO: Change to BooleanCrystal and add Type info.
                    BooleanLiteralCrystal booleanCrystal = new BooleanLiteralCrystal(stringToken);
                    Boolean booleanValue = Boolean.valueOf(stringToken);
                    booleanCrystal.setValue(booleanValue);
                    booleanCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(booleanCrystal);
                    continue;
                }

                if (TokenType.SUBTRACT.getIdentifier().equals(stringToken)) {
                    // Check if two sequential "-" tokens should be collapsed into a single ThrowCrystal.
                    String nextToken = null;
                    if (tokenNumber + 1 < statementOfStringTokens.size()) {
                        nextToken = statementOfStringTokens.get(tokenNumber + 1);
                    }
                    if (TokenType.SUBTRACT.getIdentifier().equals(nextToken)) {
                        AtonementCrystal prevCrystal = null;
                        if (!statementOfCrystals.isEmpty()) {
                            prevCrystal = statementOfCrystals.get(statementOfCrystals.size() - 1);
                        }
                        if (statementOfCrystals.isEmpty() || COLLAPSE_THROW_CRYSTAL_CLASSES.contains(prevCrystal.getClass())) {
                            ThrowCrystal throwCrystal = new ThrowCrystal();
                            throwCrystal.setCoordinates(tokenCoordinates);
                            statementOfCrystals.add(throwCrystal);
                            tokenNumber++;
                            column++;
                            continue;
                        }
                    }

                    // Otherwise, handle all as SUBTRACT at first.
                    SubtractOperatorCrystal subtractCrystal = new SubtractOperatorCrystal();
                    subtractCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(subtractCrystal);
                    continue;
                }

                // Collapse a negation operator onto a following number token, if necessary and appropriate.
                CoordinatePair negationOperatorLocation = null;
                if (isNumberToken(stringToken) && !statementOfCrystals.isEmpty()) {
                    int crystalCount = statementOfCrystals.size();
                    AtonementCrystal maybeNegationOperator = statementOfCrystals.get(crystalCount - 1);

                    if (maybeNegationOperator instanceof SubtractOperatorCrystal) {
                        AtonementCrystal previousCrystal = null;
                        if (crystalCount >= 2) {
                            previousCrystal = statementOfCrystals.get(crystalCount - 2);
                        }
                        if (previousCrystal == null || COLLAPSE_NEGATION_OPERATOR_CLASSES.contains(previousCrystal.getClass())) {
                            // Remove the negation operator crystal from the list of crystals.
                            statementOfCrystals.remove(crystalCount - 1);
                            negationOperatorLocation = maybeNegationOperator.getCoordinates();

                            // Concatenate the operator to the number token.
                            String negationOperatorToken = TokenType.NEGATE.getIdentifier();
                            stringToken = negationOperatorToken + stringToken;
                        }
                    }
                }

                // TODO: Impose maximum limits on length of Vikari number literals.

                // Handle integer number literals.
                if (Utils.isIntegerNumber(stringToken)) {
                    IntegerCrystal numberCrystal = new IntegerCrystal(stringToken, stringToken);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    numberCrystal.setNegationOperatorLocation(negationOperatorLocation);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                } else if (Utils.isLongIntegerNumber(stringToken)) {
                    String numericValue = stringToken;
                    if (Utils.hasLongSuffix(numericValue)) {
                        numericValue = Utils.trimLastCharacter(numericValue);
                    }
                    LongCrystal numberCrystal = new LongCrystal(stringToken, numericValue);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    numberCrystal.setNegationOperatorLocation(negationOperatorLocation);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                } else if (Utils.isBigIntegerNumber(stringToken)) {
                    String numericValue = stringToken;
                    if (Utils.hasBigSuffix(numericValue)) {
                        numericValue = Utils.trimLastCharacter(numericValue);
                    }
                    BigIntegerCrystal numberCrystal = new BigIntegerCrystal(stringToken, numericValue);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    numberCrystal.setNegationOperatorLocation(negationOperatorLocation);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                }

                // TODO: Impose maximum limits on length of Vikari number literals.

                // Handle decimal number literals.
                if (Utils.isDoubleNumber(stringToken)) {
                    // Double is checked first so unspecified decimal literals, aka 1.2, default to a Double type.
                    DoubleCrystal numberCrystal = new DoubleCrystal(stringToken, stringToken);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    numberCrystal.setNegationOperatorLocation(negationOperatorLocation);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                } else if (Utils.isFloatNumber(stringToken)) {
                    FloatCrystal numberCrystal = new FloatCrystal(stringToken, stringToken);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    numberCrystal.setNegationOperatorLocation(negationOperatorLocation);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                } else if (Utils.isBigDecimalNumber(stringToken)) {
                    String numericValue = stringToken;
                    if (Utils.hasBigSuffix(numericValue)) {
                        numericValue = Utils.trimLastCharacter(numericValue);
                    }
                    BigDecimal bigDecimal = new BigDecimal(numericValue, Arithmetic.getMathContext());
                    BigDecimalCrystal numberCrystal = new BigDecimalCrystal(stringToken, bigDecimal);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    numberCrystal.setNegationOperatorLocation(negationOperatorLocation);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                }

                // Handle single-line string literals.
                if (Utils.isStringLiteral(stringToken)) {
                    // TODO: Impose maximum limits on length of Vikari strings.
                    StringLiteralCrystal stringCrystal = new StringLiteralCrystal(stringToken);
                    String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
                    String contents = Utils.stripEnclosure(stringToken, captureQuotation, captureQuotation);
                    stringCrystal.setString(contents);
                    stringCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(stringCrystal);
                    continue;
                }

                // Walk multiple lines at once until end of a multi-line string literal is reached.
                if (Utils.isStartOfStringLiteral(stringToken)) {
                    // TODO: Impose maximum limits on length of Vikari strings.
                    MultiLineStringLiteralCrystal stringCrystal = new MultiLineStringLiteralCrystal(stringToken);
                    String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
                    String contents = stringToken.substring(captureQuotation.length());
                    stringCrystal.setString(contents);
                    stringCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(stringCrystal);
                    MultiLineStringLiteralCrystal prevStringCrystal = stringCrystal;
                    boolean atFirstTokenAsCaptureQuotation = stringToken.equals(captureQuotation);
                    while ((atFirstTokenAsCaptureQuotation || !Utils.isEndOfStringLiteral(stringToken)) &&
                            statementNumber < statementsOfStringTokens.size() - 1) {
                        atFirstTokenAsCaptureQuotation = false;
                        statementNumber++;
                        tokenNumber = 0;
                        column = 0;
                        row = startLineNumber + statementNumber;
                        tokenCoordinates = new CoordinatePair(row, column);
                        statementOfStringTokens = statementsOfStringTokens.get(statementNumber);
                        statementsOfCrystals.add(statementOfCrystals);
                        statementOfCrystals = new ArrayList<>();
                        stringToken = statementOfStringTokens.get(tokenNumber);
                        MultiLineStringLiteralCrystal nextStringCrystal = new MultiLineStringLiteralCrystal(stringToken);
                        contents = stringToken;
                        if (Utils.isEndOfStringLiteral(stringToken)) {
                            contents = stringToken.substring(0, stringToken.length() - captureQuotation.length());
                        }
                        nextStringCrystal.setString(contents);
                        nextStringCrystal.setCoordinates(tokenCoordinates);
                        statementOfCrystals.add(nextStringCrystal);
                        prevStringCrystal.setNext(nextStringCrystal);
                        prevStringCrystal = nextStringCrystal;
                    }
                    continue;
                }

                if (Utils.isSingleLineComment(stringToken)) {
                    // Single-line comments are omitted in the final output of the Lexer.
                    continue;
                }

                if (Utils.isStartOfComment(stringToken)) {
                    // Multi-line comments are omitted in the final output of the Lexer.
                    while (!Utils.isEndOfComment(stringToken) && statementNumber < statementsOfStringTokens.size() - 1) {
                        statementNumber++;
                        tokenNumber = 0;
                        column = 0;
                        statementOfStringTokens = statementsOfStringTokens.get(statementNumber);
                        stringToken = statementOfStringTokens.get(tokenNumber);
                    }
                    continue;
                }

                if (TokenType.CAPTURE_QUOTATION.getIdentifier().equals(stringToken)) {
                    CaptureQuotationCrystal captureQuotationCrystal = new CaptureQuotationCrystal();
                    captureQuotationCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(captureQuotationCrystal);
                    continue;
                }

                if (Utils.isBacktickQuotedIdentifier(stringToken)) {
                    // TODO: Impose maximum limits on length of singular-backtick-quoted identifiers.
                    // Resolve actual type information at a later step.
                    ReferenceCrystal any = new ReferenceCrystal(stringToken);
                    any.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(any);
                    continue;
                }

                if (Utils.isTypeIdentifier(stringToken)) {
                    // TODO: Impose maximum limits on length of variable-length Vikari identifiers.
                    // Resolve actual type information at a later step.
                    TypeReferenceCrystal any = new TypeReferenceCrystal(stringToken);
                    any.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(any);
                    continue;
                }

                if (Utils.isSword(stringToken)) {
                    // TODO: Impose maximum limits on length of variable-length Vikari identifiers.
                    SwordCrystal swordCrystal = new SwordCrystal(stringToken);
                    swordCrystal.setLength(stringToken.length());
                    swordCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(swordCrystal);
                    continue;
                }

                if (TokenType.MODULUS.getIdentifier().equals(stringToken)) {
                    // Handle all as MODULUS at first.
                    ModulusOperatorCrystal modulusCrystal = new ModulusOperatorCrystal();
                    modulusCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(modulusCrystal);
                    continue;
                }

                if (TokenType.DELETE.getIdentifier().equals(stringToken)) {
                    // Handle all as DELETE at first.
                    DeleteOperatorCrystal deleteOperatorCrystal = new DeleteOperatorCrystal();
                    deleteOperatorCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(deleteOperatorCrystal);
                    continue;
                }

                if (TokenType.MULTIPLY.getIdentifier().equals(stringToken)) {
                    // Handle all as MULTIPLY at first.
                    MultiplyOperatorCrystal multiplyCrystal = new MultiplyOperatorCrystal();
                    multiplyCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(multiplyCrystal);
                    continue;
                }

                if (TokenType.ADD.getIdentifier().equals(stringToken)) {
                    // Handle all as ADD at first.
                    AddOperatorCrystal addCrystal = new AddOperatorCrystal();
                    addCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(addCrystal);
                    continue;
                }

                if (TokenType.CONTINUE.getIdentifier().equals(stringToken)) {
                    // handle all as CONTINUE at first.
                    ContinueOperatorCrystal addCrystal = new ContinueOperatorCrystal();
                    addCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(addCrystal);
                    continue;
                }

                // Blank lines are ignored in the output of the Lexer.
                if (stringToken.equals("")) {
                    continue;
                }

                TokenType mapping = defaultIdentifiersMap.get(stringToken);

                // Not a built-in type. Resolve concrete instance of identifier in parsing step.
                if (mapping == null) {
                    // TODO: Impose maximum limits on length of Vikari identifiers.
                    if (Utils.isCrystalIdentifier(stringToken) || Utils.isFieldRegionIdentifier(stringToken)) {
                        ReferenceCrystal any = new ReferenceCrystal(stringToken);
                        any.setCoordinates(tokenCoordinates);
                        statementOfCrystals.add(any);
                        continue;
                    } else if (isInvalidToken(stringToken)) {
                        continue;
                    } else {
                        ReferenceCrystal any = new ReferenceCrystal(stringToken);
                        any.setCoordinates(tokenCoordinates);
                        statementOfCrystals.add(any);
                        continue;
                    }
                }

                // One of the default identifier types of the Vikari language.
                // Build a new instance of its crystal type using reflection.
                try {
                    Class<? extends AtonementCrystal> clazz = mapping.getJavaType();
                    Constructor<?> constructor = clazz.getConstructor();
                    AtonementCrystal crystal = (clazz.cast(constructor.newInstance()));
                    crystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(crystal);
                } catch (NoSuchMethodException | InvocationTargetException | InstantiationException |
                         IllegalAccessException e) {
                    throw new Vikari_LexerException("Internal error. Description: \"" + e.getMessage() + "\"");
                }
            }

            // Ensure blank lines contain a single expected crystal to preserve the line number.
            if (!statementOfCrystals.isEmpty()) {
                statementsOfCrystals.add(statementOfCrystals);
            }
        }

        return statementsOfCrystals;
    }

    private String getPreviousNonWhitespaceToken(List<String> stringTokens, int tokenNumber) {
        for (int i = tokenNumber - 1; i >= 0; i--) {
            String token = stringTokens.get(i);
            if (!Utils.isWhitespace(token)) {
                return token;
            }
        }
        return null;
    }

    private int getNextNonWhitespaceTokenNumber(List<String> stringTokens, int tokenNumber) {
        for (int i = tokenNumber + 1; i < stringTokens.size(); i++) {
            String token = stringTokens.get(i);
            if (!Utils.isWhitespace(token)) {
                return i;
            }
        }
        return -1;
    }

    private boolean isNumberToken(String token) {
        Matcher matcher = numberRegex.matcher(token);
        return matcher.matches();
    }

    private boolean isInvalidToken(String token) {
        Matcher matcher = invalidCharactersRegex.matcher(token);
        return matcher.matches();
    }

    /**
     * Sum the lengths of all tokens in the given range.
     * @param stringTokens The list of tokens against which to sum the lengths across the given range.
     * @param startTokenNumber The start of the range (inclusive).
     * @param endTokenNumber The end of the range. (exclusive).
     * @return The sum of all tokens in the given range.
     */
    private int getTokenWidths(List<String> stringTokens, int startTokenNumber, int endTokenNumber) {
        int sum = 0;
        for (int i = startTokenNumber; i < endTokenNumber; i++) {
            String token = stringTokens.get(i);
            sum += token.length();
        }
        return sum;
    }


}
