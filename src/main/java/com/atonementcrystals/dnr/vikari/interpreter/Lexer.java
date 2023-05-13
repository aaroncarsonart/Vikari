package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.comment.CommentCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.comment.MultiLineCommentCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.ReferenceCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Performs the lexical analysis step of scanning the contents of a
 * Vikari source file or string and splitting it into string tokens.
 * These tokens are then converted to their associated crystal types
 * as the final output of the Lexer.
 */
public class Lexer {
    private static final Logger log = LogManager.getLogger(Lexer.class);

    private SyntaxErrorReporter syntaxErrorReporter;
    private File currentFile;

    public void setSyntaxErrorReporter(SyntaxErrorReporter syntaxErrorReporter) {
        this.syntaxErrorReporter = syntaxErrorReporter;
    }

    /**
     * Lexes a Vikari source file into a sequence of AtonementCrystals.
     *
     * @param sourceFile The Vikari source file to lex.
     * @return The sequence of AtonementCrystals defined by the Vikari source file.
     */
    public List<List<AtonementCrystal>> lexVikariSourceFile(File sourceFile) {
        log.trace("lexVikariSourceFile()");
        currentFile = sourceFile;

        List<List<String>> statementsOfStringTokens = lexToStringTokens(sourceFile);
        statementsOfStringTokens = collapseTokens(statementsOfStringTokens);
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
    public List<List<AtonementCrystal>> lexVikariSourceCode(String sourceCode) {
        log.trace("lexVikariSourceCode()");
        currentFile = null;
        List<List<String>> statementsOfStringTokens = lexToStringTokens(sourceCode);
        statementsOfStringTokens = collapseTokens(statementsOfStringTokens);
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
        log.trace("lexToStringTokens(sourceFile)");
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
        log.trace("lexToStringTokens(sourceString)");
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
    private List<List<String>> readFromBufferAsStringTokens(BufferedReader reader) throws IOException {
        List<List<String>> statementsOfStringTokens = new ArrayList<>();
        String line;
        while ((line = reader.readLine()) != null) {
            List<String> stringTokens = tokenize(line);
            statementsOfStringTokens.add(stringTokens);
        }
        return statementsOfStringTokens;
    }

    /**
     * Performs the initial string tokenization step of the Lexer upon a single
     * line of Vikari source code. Requires collapsion and conversion into its
     * associated crystal types before the lexing step is complete.
     *
     * @param line The line of Vikari source code to tokenize.
     * @return A list of string tokens.
     */
    public List<String> tokenize(String line) {
        BinaryStringTokenTree binaryTree = new BinaryStringTokenTree(line);
        List<String> stringTokens = binaryTree.listFrominOrderTraversal();
        return stringTokens;
    }

    /**
     * Collapses tokens together which represent more complex structures, such as strings,
     * quoted identifiers, comments, decimal numbers, and whitespace.
     *
     * @param statementsOfStringTokens The list of strint tokens to collapse.
     * @return A new list of collapsed statements.
     */
    public List<List<String>> collapseTokens(List<List<String>> statementsOfStringTokens) {
        log.trace("collapseTokens()");
        int numberOfLines = statementsOfStringTokens.size();

        String commentPrefix = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        String commentSuffix = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();
        String backtickQuotation = TokenType.BACKTICK.getIdentifier();
        String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();

        boolean stop = false;
        List<List<String>> collapsedLines = new ArrayList<>();
        for (int lineNumber = 0; lineNumber <  numberOfLines; lineNumber++) {
            List<String> line = statementsOfStringTokens.get(lineNumber);
            List<String> collapsedLine = new ArrayList<>();

            String nextToken;
            for (int tokenNumber = 0; tokenNumber < line.size(); tokenNumber++) {
                String token = line.get(tokenNumber);

                // ----------------------
                // 1: Collapse comments.
                // ----------------------
                if (token.equals(commentPrefix)) {
                    int startLine = lineNumber;
                    int startTokenNumber = tokenNumber;
                    CollapseEnclosureResult result = collapseEnclosure(commentPrefix, commentSuffix, tokenNumber, line);
                    tokenNumber = result.columnIndex;
                    nextToken = result.collapsedString;
                    boolean finished = result.finished;
                    while (!finished) {
                        collapsedLine.add(nextToken);
                        collapsedLines.add(collapsedLine);
                        lineNumber++;

                        if (lineNumber < numberOfLines) {
                            collapsedLine = new ArrayList<>();
                            tokenNumber = 0;
                            line = statementsOfStringTokens.get(lineNumber);
                            result = collapseEnclosure(null, commentSuffix, tokenNumber, line);
                            tokenNumber = result.columnIndex;
                            nextToken = result.collapsedString;
                            finished = result.finished;
                        } else {
                            // Report syntax error if missing comment suffix `:~` token.
                            stop = true;
                            String errorMessage = "Missing comment suffix token `:~`.";
                            int errorLineNumber = startLine;
                            int errorTokenNumber = startTokenNumber;
                            reportError(errorMessage, errorLineNumber, errorTokenNumber, statementsOfStringTokens);
                            break;
                        }
                    }
                }

                // --------------------------------
                // 2: Collapse capture quotations.
                // --------------------------------
                else if (token.equals(captureQuotation)) {
                    int startLine = lineNumber;
                    int startTokenNumber = tokenNumber;
                    CollapseEnclosureResult result = collapseEnclosure(captureQuotation, captureQuotation, tokenNumber, line);
                    tokenNumber = result.columnIndex;
                    nextToken = result.collapsedString;
                    boolean finished = result.finished;
                    while (!finished) {
                        collapsedLine.add(nextToken);
                        collapsedLines.add(collapsedLine);
                        lineNumber++;


                        if (lineNumber < numberOfLines) {
                            collapsedLine = new ArrayList<>();
                            tokenNumber = 0;
                            line = statementsOfStringTokens.get(lineNumber);
                            result = collapseEnclosure(null, captureQuotation, tokenNumber, line);
                            tokenNumber = result.columnIndex;
                            nextToken = result.collapsedString;
                            finished = result.finished;
                        } else {
                            // Report an error if missing ending capture quotation `` token.
                            stop = true;
                            String errorMessage = "Missing closing capture quotation ``.";
                            int errorLineNumber = startLine;
                            int errorTokenNumber = startTokenNumber;
                            reportError(errorMessage, errorLineNumber, errorTokenNumber, statementsOfStringTokens);
                            break;
                        }
                    }
                }

                // ---------------------------------
                // 3: Collapse backtick quotations.
                // ---------------------------------
                else if (token.equals(backtickQuotation)) {
                    int backtickTokenNumber = tokenNumber;
                    CollapseEnclosureResult result = collapseEnclosure(backtickQuotation, backtickQuotation, tokenNumber, line);
                    tokenNumber = result.columnIndex;
                    nextToken = result.collapsedString;
                    boolean finished = result.finished;

                    if (!finished) {
                        // Report an error if missing ending backtick quotation ` token.
                        String errorMessage = "Missing closing backtick quotation `.";
                        int errorLineNumber = lineNumber;
                        int errorTokenNumber = backtickTokenNumber;
                        reportError(errorMessage, errorLineNumber, errorTokenNumber, statementsOfStringTokens);
                    } else {
                        String unquotedNextToken = Utils.stripEnclosure(nextToken, backtickQuotation, backtickQuotation);
                        boolean isSpaceCharLiteral = unquotedNextToken.equals(" ");
                        if (!isSpaceCharLiteral && Utils.isWhitespace(unquotedNextToken)) {
                            // Report an error if identifier contains only whitespace.
                            String errorMessage = "Backtick-quoted identifiers cannot contain only whitespace.";
                            int errorLineNumber = lineNumber;
                            int errorTokenNumber = backtickTokenNumber + 1;
                            reportError(errorMessage, errorLineNumber, errorTokenNumber, statementsOfStringTokens);
                        } else if (unquotedNextToken.contains("\t")) {
                            // Report an error if identifier contains a tab character.
                            String errorMessage = "Backtick-quoted identifiers cannot contain tabs.";
                            int errorLineNumber = lineNumber;
                            int errorTokenNumber = backtickTokenNumber + 1;
                            reportError(errorMessage, errorLineNumber, errorTokenNumber, statementsOfStringTokens);
                        }
                    }
                }

                // -------------------------------------------
                // 4: Collapse decimal number literal values.
                // -------------------------------------------
                else if (Utils.isBigIntegerNumber(token)) {
                    if (tokenNumber + 2 < line.size()) {
                        String maybeDot = line.get(tokenNumber + 1);
                        String maybeInteger = line.get(tokenNumber + 2);
                        if (maybeDot.equals(".") && Utils.isValidDecimalFractionalPart(maybeInteger)) {
                            String newDecimalToken = token + maybeDot + maybeInteger;
                            nextToken = newDecimalToken;
                            tokenNumber += 2;
                        } else {
                            nextToken = token;
                        }
                    } else {
                        nextToken = token;
                    }
                }

                // ------------------------
                // 5: Collapse whitespace.
                // ------------------------
                else if (Utils.isWhitespace(token)) {
                    StringBuilder sb = new StringBuilder();
                    sb.append(token);

                    int i = tokenNumber + 1;
                    while (i < line.size()) {
                        String followingToken = line.get(i);
                        if (Utils.isWhitespace(followingToken)) {
                            sb.append(followingToken);
                            i++;
                        } else {
                            break;
                        }
                    }

                    String whitespace = sb.toString();
                    nextToken = whitespace;

                    // The algorithm always walks one more step forward than necessary.
                    // So back up one!
                    tokenNumber = i - 1;
                }

                // ----------------------------------------------
                // 6: Regular token, so just add it to the list.
                // ----------------------------------------------
                else {
                    nextToken = token;
                }

                // Short-circuit for when end of file has already been reached.
                if (stop) {
                    break;
                }

                collapsedLine.add(nextToken);
            }

            // Short-circuit for when end of file has already been reached.
            if (stop) {
                break;
            }

            // ------------------------------------------------------------
            // 7: Make a second-pass to collapse negative number literals.
            // ------------------------------------------------------------
            List<String> negativeNumbersCollapsedLine = new ArrayList<>();

            for (int tokenNumber = 0; tokenNumber < collapsedLine.size(); tokenNumber++) {
                String token = collapsedLine.get(tokenNumber);
                // Set default value now to cover all else-cases.
                nextToken = token;
                if (token.equals(TokenType.NEGATE.getIdentifier())) {
                    if (tokenNumber < collapsedLine.size() - 1) {
                        String followingToken = collapsedLine.get(tokenNumber + 1);
                        if (Utils.isBigDecimalNumber(followingToken) || Utils.isBigIntegerNumber(followingToken)) {
                            nextToken = token + followingToken;
                            tokenNumber++;
                        }
                    }
                }
                negativeNumbersCollapsedLine.add(nextToken);
            }
            collapsedLine = negativeNumbersCollapsedLine;
            collapsedLines.add(collapsedLine);
        }

        return collapsedLines;
    }

    private void reportError(String message, int lineNumber, int tokenNumber, List<List<String>> statementsOfStringTokens) {
        List<String> errorStatement = statementsOfStringTokens.get(lineNumber);
        String lineForError = errorStatement.stream().collect(Collectors.joining());
        int row = lineNumber;
        int column = 0;
        for (int i = 0; i < tokenNumber; i++) {
            String tmpToken = errorStatement.get(i);
            column += tmpToken.length();
        }
        CoordinatePair location = new CoordinatePair(row, column);
        SyntaxError syntaxError = new SyntaxError(currentFile, location, lineForError, message);
        syntaxErrorReporter.add(syntaxError);
    }

    /**
     * For holding the result of the collapseTokens method.
     */
    class CollapseEnclosureResult {
        int columnIndex;
        String collapsedString;
        boolean finished;
        CollapseEnclosureResult(int columnIndex, String collapsedString, boolean finished) {
            this.columnIndex = columnIndex;
            this.collapsedString = collapsedString;
            this.finished = finished;
        }
    }

    /**
     * Collapses an enclosure of string tokens based on the start and end strings provided.
     *
     * @param startOfEnclosure The beginning of the enclosure.
     * @param endOfEnclosure The ending of the enclosure.
     * @param startIndex The index of what string token to begin collapsing at.
     * @param lineOfStringTokens The current line of string tokens being collapsed.
     *
     * @return A CollapseEnclosureResult holding all data of the result of collapsing this
     *         region of the line of string tokens.
     */
    public CollapseEnclosureResult collapseEnclosure(String startOfEnclosure, String endOfEnclosure, int startIndex, List<String> lineOfStringTokens) {
        int index = startIndex;
        StringBuilder sb = new StringBuilder();

        if (startOfEnclosure != null) {
            sb.append(startOfEnclosure);
            index++;
        }

        String nextToken = null;
        for (int i = index; i < lineOfStringTokens.size() && !endOfEnclosure.equals(nextToken); i++) {
            nextToken = lineOfStringTokens.get(i);
            sb.append(nextToken);
            index = i;
        }

        int columnIndex = index;
        String collapsedString = sb.toString();
        boolean finished = endOfEnclosure.equals(nextToken);
        return new CollapseEnclosureResult(columnIndex, collapsedString, finished);
    }

    /**
     * Converts string tokens into their associated crystal types.
     * Identifiers which are references will be resolved later by the parser.
     *
     * @param statementsOfStringTokens The string tokens to convert.
     * @return The crystals representing each string token.
     */
    public List<List<AtonementCrystal>> convertTokensToCrystals(List<List<String>> statementsOfStringTokens) {
        log.trace("convertTokensToCrystals()");
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

                // advance column number only once
                if (tokenNumber > 0) {
                    String previousToken = statementOfStringTokens.get(tokenNumber - 1);
                    column += previousToken.length();
                }

                CoordinatePair tokenCoordinates = new CoordinatePair(statementNumber, column);

                // handle whitespace
                if (Utils.isWhitespace(stringToken)) {
                    // TODO: Impose maximum limits on length of variable-length Vikari identifiers.
                    // TODO: Impose maximum limits on indentation levels in Vikari.
                    WhitespaceCrystal whitespaceCrystal = new WhitespaceCrystal(stringToken);
                    whitespaceCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(whitespaceCrystal);
                    continue;
                }

                if (Utils.isBooleanLiteral(stringToken)) {
                    BooleanLiteralCrystal booleanCrystal = new BooleanLiteralCrystal(stringToken);
                    Boolean booleanValue = Boolean.valueOf(stringToken);
                    booleanCrystal.setValue(booleanValue);
                    booleanCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(booleanCrystal);
                    continue;
                }

                // TODO: Impose maximum limits on length of Vikari number literals.

                // handle integer number literals
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

                // handle decimal number literals
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

                // handle single-line string literals
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
                    String contents = stringToken.substring(captureQuotation.length(), stringToken.length());
                    stringCrystal.setString(contents);
                    stringCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(stringCrystal);
                    MultiLineStringLiteralCrystal prevStringCrystal = stringCrystal;
                    boolean AtFirstTokenAsCaptureQuotation = stringToken.equals(captureQuotation);
                    while ((AtFirstTokenAsCaptureQuotation || !Utils.isEndOfStringLiteral(stringToken)) &&
                            statementNumber < statementsOfStringTokens.size() - 1) {
                        AtFirstTokenAsCaptureQuotation = false;
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
                    String contents = stringToken.substring(commentPrefixOperator.length(), stringToken.length());
                    stringCrystal.setComment(contents);
                    stringCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(stringCrystal);
                    MultiLineCommentCrystal prevCommentCrystal = stringCrystal;
                    while (!Utils.isEndOfStringLiteral(stringToken) &&
                            statementNumber < statementsOfStringTokens.size() - 1) {
                        statementNumber++;
                        tokenNumber = 0;
                        tokenCoordinates = new CoordinatePair(statementNumber, 0);
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

                if (Utils.isSword(stringToken)) {
                    // TODO: Impose maximum limits on length of variable-length Vikari identifiers.
                    SwordCrystal swordCrystal = new SwordCrystal(stringToken);
                    swordCrystal.setLength(stringToken.length());
                    swordCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(swordCrystal);
                    continue;
                }

                if (TokenType.MODULUS.getIdentifier().equals(stringToken)) {
                    // handle all as MODULUS at first.
                    ModulusOperatorCrystal modulusCrystal = new ModulusOperatorCrystal();
                    modulusCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(modulusCrystal);
                    continue;
                }

                if (TokenType.DELETE.getIdentifier().equals(stringToken)) {
                    // handle all as DELETE at first.
                    DeleteOperatorCrystal deleteOperatorCrystal = new DeleteOperatorCrystal();
                    deleteOperatorCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(deleteOperatorCrystal);
                    continue;
                }

                if (TokenType.SUBTRACT.getIdentifier().equals(stringToken)) {
                    // handle all as SUBTRACT at first.
                    SubtractOperatorCrystal subtractCrystal = new SubtractOperatorCrystal();
                    subtractCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(subtractCrystal);
                    continue;
                }

                if (TokenType.MULTIPLY.getIdentifier().equals(stringToken)) {
                    // handle all as MULTIPLY at first.
                    MultiplyOperatorCrystal multiplyCrystal = new MultiplyOperatorCrystal();
                    multiplyCrystal.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(multiplyCrystal);
                    continue;
                }

                if (TokenType.ADD.getIdentifier().equals(stringToken)) {
                    // handle all as ADD at first.
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

                // Insert a blank line crystal as a placeholder to preserve the line numbers
                // of the following statements.
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
                    ReferenceCrystal any = new ReferenceCrystal(stringToken);
                    any.setCoordinates(tokenCoordinates);
                    statementOfCrystals.add(any);
                    continue;
                }

                // One of the default identifier types of the Vikari language.
                // Build a new instance of its crystal type using reflection.
                try {
                    Class<? extends AtonementCrystal> clazz = mapping.getType();
                    Class<?>[] constructorParameters = {};
                    Constructor<?> constructor = clazz.getConstructor(constructorParameters);
                    AtonementCrystal crystal = (clazz.cast(constructor.newInstance(constructorParameters)));
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
}