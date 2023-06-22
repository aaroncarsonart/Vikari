package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.comment.CommentCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.comment.MultiLineCommentCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TypeReferenceCrystal;
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
import com.atonementcrystals.dnr.vikari.core.crystal.separator.BlankLineCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.separator.WhitespaceCrystal;
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

    private static final Pattern whitespaceRegex = Pattern.compile("^[ \t]+");
    private static final Pattern numberRegex = Pattern.compile("^\\d+(?:\\.\\d+)?(?i)[LFDB]?");
    private static final Pattern identifierRegex = Pattern.compile("^[A-Za-z_]\\w*");
    private static final Pattern invalidCharactersRegex = Pattern.compile("^[^\t -~]+");

    /**
     * If a negation operator preceding a number literal follows one of these tokens,
     * the operator should be collapsed together with the number literal.
     */
    private static final HashSet<String> COLLAPSE_NEGATION_OPERATOR_TOKENS = Stream.of(
                    TokenType.RETURN, TokenType.BREAK, TokenType.CONTINUE, TokenType.LEFT_SQUARE_BRACKET,
                    TokenType.STATEMENT_SEPARATOR, TokenType.REGION_SEPARATOR, TokenType.LEFT_PARENTHESIS,
                    TokenType.LIST_ELEMENT_SEPARATOR, TokenType.RANGE, TokenType.PRINT_STATEMENT,
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
            .map(TokenType::getIdentifier)
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

    List<String> lines;
    List<String> stringTokens;
    List<List<String>> statementsOfStringTokens;

    public void setSyntaxErrorReporter(SyntaxErrorReporter syntaxErrorReporter) {
        this.syntaxErrorReporter = syntaxErrorReporter;
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
        lines = new ArrayList<>();

        line = readNextLine();
        lineLength = atEndOfFile() ? 0 : line.length();

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
                        tryMatchAndGetToken("<<", ">>", "-", ">");
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
        cacheLineData();
        return statementsOfStringTokens;
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

    private void cacheLineData() {
        if (syntaxErrorReporter == null) {
            syntaxErrorReporter = new SyntaxErrorReporter();
        }
        syntaxErrorReporter.addLinesForFile(currentFile, lines);
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
        String errorLine = line;

        // Consume the string token(s).
        boolean consumedClosingToken = multilineToken(TokenType.CAPTURE_QUOTATION.getIdentifier());

        // Report if there was an error.
        if (!consumedClosingToken) {
            String errorMessage = "Missing closing capture quotation ``.";
            reportError(errorMessage, errorLineNumber, errorStart, errorLine);
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
            reportError("Missing closing backtick quotation `.", lineNumber, startIndex, line);
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
                reportError(errorMessage, lineNumber, startIndex + 1, line);
            } else if (unquotedNextToken.contains("\t")) {
                // Report an error if identifier contains a tab character.
                String errorMessage = "Backtick-quoted identifiers cannot contain tabs.";
                reportError(errorMessage, lineNumber, startIndex + 1, line);
            }
        }
    }

    private void comment() {
        // Cache values in case of a syntax error.
        int errorLineNumber = lineNumber;
        int errorStart = startIndex;
        String errorLine = line;

        // Consume the comment token(s).
        String commentPrefix = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        String commentSuffix = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();
        boolean consumedClosingToken = multilineToken(commentPrefix, commentSuffix);

        // Report if there was an error.
        if (!consumedClosingToken) {
            String errorMessage = "Missing comment suffix token `:~`.";
            reportError(errorMessage, errorLineNumber, errorStart, errorLine);
        }
    }

    /**
     * Consumes characters until either the closing token or the end of file is reached.
     * @param closingToken The string which ends this multi-line token.
     * @return True if the closing token was reached, else false.
     */
    private boolean multilineToken(String closingToken) {
        return multilineToken(null, closingToken);
    }

    /**
     * Consumes characters until either the closing token or the end of file is reached.
     * @param openingToken The String which started this multi-line token. (Not used until nesting is supported.)
     * @param closingToken The string which ends this multi-line token.
     * @return True if the closing token was reached, else false.
     */
    private boolean multilineToken(String openingToken, String closingToken) {
        advance();

        // TODO: Allow for nesting of comments if openingToken != null. (In a future commit.)

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
            reportError(INVALID_CHARACTERS_ERROR_MESSAGE, lineNumber, startIndex, line);
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
     * @param lineForError The String of the line for which the error occurred.
     */
    private void reportError(String message, int row, int column, String lineForError) {
        CoordinatePair location = new CoordinatePair(row, column);
        SyntaxError syntaxError = new SyntaxError(currentFile, location, lineForError, message);
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

                CoordinatePair tokenCoordinates = new CoordinatePair(statementNumber, column);

                if (Utils.isWhitespace(stringToken)) {
                    // TODO: Impose maximum limits on length of variable-length Vikari identifiers.
                    // TODO: Impose maximum limits on indentation levels in Vikari.
                    WhitespaceCrystal whitespaceCrystal = new WhitespaceCrystal(stringToken);
                    whitespaceCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(whitespaceCrystal);
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

                // NOTE: This check must happen before number tokens are processed.
                if (TokenType.SUBTRACT.getIdentifier().equals(stringToken)) {
                    // Check if a negation operator needs to be collapsed into a following number token.

                    // TODO: Move negation operator collapsion to a second pass after the entire line has been processed
                    //       in this method once whitespace and comments have been elided from the Lexer output in a
                    //       future commit.

                    int followingTokenNumber = getNextNonWhitespaceTokenNumber(statementOfStringTokens, tokenNumber);
                    String followingToken = null;

                    if (followingTokenNumber != -1) {
                        followingToken = statementOfStringTokens.get(followingTokenNumber);
                    }

                    boolean isCollapsedNegationOperator = false;
                    if (followingToken != null && isNumberToken(followingToken)) {
                        // Check if the previous token implies this is a negation operator to be collapsed.
                        String previousToken = getPreviousNonWhitespaceToken(statementOfStringTokens, tokenNumber);

                        // The null case means this is the first token in a statement, and so collapsion is allowed.
                        if (previousToken == null || COLLAPSE_NEGATION_OPERATOR_TOKENS.contains(previousToken)) {
                            // Combine the negation operator with the number token. This will then be handled correctly
                            // as a single token when number tokens are processed later on in this same method.
                            stringToken += followingToken;
                            column += getTokenWidths(statementOfStringTokens, tokenNumber, followingTokenNumber);
                            tokenCoordinates = new CoordinatePair(statementNumber, column);
                            tokenNumber = followingTokenNumber;
                            isCollapsedNegationOperator = true;
                        }
                    }

                    if (!isCollapsedNegationOperator) {
                        // handle all as SUBTRACT at first.
                        SubtractOperatorCrystal subtractCrystal = new SubtractOperatorCrystal();
                        subtractCrystal.setCoordinates(tokenCoordinates);
                        statementOfCrystals.add(subtractCrystal);
                        continue;
                    }
                }

                // TODO: Impose maximum limits on length of Vikari number literals.

                // Handle integer number literals.
                if (Utils.isIntegerNumber(stringToken)) {

                    IntegerCrystal numberCrystal = new IntegerCrystal(stringToken, stringToken);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                } else if (Utils.isLongIntegerNumber(stringToken)) {
                    String numericValue = stringToken;
                    if (Utils.hasLongSuffix(numericValue)) {
                        numericValue = Utils.trimLastCharacter(numericValue);
                    }
                    LongCrystal numberCrystal = new LongCrystal(stringToken, numericValue);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                } else if (Utils.isBigIntegerNumber(stringToken)) {
                    String numericValue = stringToken;
                    if (Utils.hasBigSuffix(numericValue)) {
                        numericValue = Utils.trimLastCharacter(numericValue);
                    }
                    BigIntegerCrystal numberCrystal = new BigIntegerCrystal(stringToken, numericValue);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                }

                // TODO: Impose maximum limits on length of Vikari number literals.

                // Handle decimal number literals.
                if (Utils.isDoubleNumber(stringToken)) {
                    // Double is checked first so unspecified decimal literals, aka 1.2, default to a Double type.
                    DoubleCrystal numberCrystal = new DoubleCrystal(stringToken, stringToken);
                    numberCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(numberCrystal);
                    continue;
                } else if (Utils.isFloatNumber(stringToken)) {
                    FloatCrystal numberCrystal = new FloatCrystal(stringToken, stringToken);
                    numberCrystal.setCoordinates(tokenCoordinates);
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
                        tokenCoordinates = new CoordinatePair(statementNumber, column);
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
                    // TODO: Impose maximum limits on length of variable-length Vikari identifiers.
                    CommentCrystal commentCrystal = new CommentCrystal(stringToken);
                    String commentPrefix = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
                    String commentSuffix = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();
                    String contents = Utils.stripEnclosure(stringToken, commentPrefix, commentSuffix);
                    commentCrystal.setComment(contents);
                    commentCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(commentCrystal);
                    continue;
                }

                if (Utils.isStartOfComment(stringToken)) {
                    // TODO: Impose maximum limits on length of variable-length Vikari identifiers.
                    MultiLineCommentCrystal stringCrystal = new MultiLineCommentCrystal(stringToken);
                    String commentPrefixOperator = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
                    String commentSuffixOperator = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();
                    String contents = stringToken.substring(commentPrefixOperator.length());
                    stringCrystal.setComment(contents);
                    stringCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(stringCrystal);
                    MultiLineCommentCrystal prevCommentCrystal = stringCrystal;
                    while (!Utils.isEndOfComment(stringToken) && statementNumber < statementsOfStringTokens.size() - 1) {
                        statementNumber++;
                        tokenNumber = 0;
                        column = 0;
                        tokenCoordinates = new CoordinatePair(statementNumber, column);
                        statementOfStringTokens = statementsOfStringTokens.get(statementNumber);
                        statementsOfCrystals.add(statementOfCrystals);
                        statementOfCrystals = new ArrayList<>();
                        stringToken = statementOfStringTokens.get(tokenNumber);
                        MultiLineCommentCrystal nextCommentCrystal = new MultiLineCommentCrystal(stringToken);
                        contents = stringToken;
                        if (Utils.isEndOfComment(stringToken)) {
                            contents = stringToken.substring(0, stringToken.length() - commentSuffixOperator.length());
                        }
                        nextCommentCrystal.setComment(contents);
                        nextCommentCrystal.setCoordinates(tokenCoordinates);
                        statementOfCrystals.add(nextCommentCrystal);
                        prevCommentCrystal.setNext(nextCommentCrystal);
                        prevCommentCrystal = nextCommentCrystal;
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

                // Insert a blank line crystal as a placeholder to preserve the line numbers of the following statements.
                if (stringToken.equals("")) {
                    BlankLineCrystal blankLineCrystal = new BlankLineCrystal();
                    blankLineCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(blankLineCrystal);
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
            statementsOfCrystals.add(statementOfCrystals);
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
