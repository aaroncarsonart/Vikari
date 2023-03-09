package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.Statement;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;
import com.atonement.crystals.dnr.vikari.error.Vikari_IOException;
import com.atonement.crystals.dnr.vikari.util.Pair;
import com.atonement.crystals.dnr.vikari.util.Utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Performs the lexical analysis step of scanning the raw text input of
 * a DNR file and splitting each statement into a sequence of individually
 * meaningful AtonementCrystal string tokens.
 */
public class Lexer {

    private void printLineOfChars(char c, int length) {
        for (int i = 0; i < length + 8; i++) {
            System.out.print(c);
        }
        System.out.println();
    }

    /**
     * Loads a new AtonementCrystal definition from a DNR source file.
     * @param sourceFile The input DNR source file to evaluate.
     * @return The new AtonementCrystal defined by the DNR source file.
     */
    public List<Statement> analyzeAtonementCrystalDefinitionFile(File sourceFile) {

        // -----------------------------------------------------
        // 1. Print full path of current file being interpreted.
        // -----------------------------------------------------
        printLineOfChars('#', sourceFile.getAbsolutePath().length() + 8);
        System.out.println("file: \"" + sourceFile.getAbsolutePath() + "\"");
        printLineOfChars('#', sourceFile.getAbsolutePath().length() + 8);
        System.out.println();

        // ----------------------------------------------------
        // 2. Parse file contents into a list of string tokens.
        // ----------------------------------------------------
        List<List<String>> statementStringTokensList = readFileAsBasicStringTokens(sourceFile);
        statementStringTokensList = collapseEnclosuresOfStringTokens(statementStringTokensList);

        return null;
    }

    /**
     * Basic tokenization of a line of text from a DNR source file by splitting the string
     * on known default tokens of the Vikari language. Output from this method will need
     * to be processed to stitch back together sequences of tokens that should not be
     * separate from one another (such as those between comment or backtick quotation
     * crystals).
     * @param line The line of Vikari to tokenize.
     * @return A list of string tokens.
     */
    public List<String> tokenize(String line) {

        Map<String, DefaultIdentifierMapping> defaultIdentifiersMap = new LinkedHashMap<>();
        List<String> defaultIdentifiersList = new ArrayList<>();
        for (DefaultIdentifierMapping identifierMapping : DefaultIdentifierMapping.values()) {
            defaultIdentifiersMap.put(identifierMapping.getIdentifier(), identifierMapping);
            defaultIdentifiersList.add(identifierMapping.getIdentifier());
        }

        // split on default identifiers
        BinaryParseTree binaryParseTree = new BinaryParseTree(defaultIdentifiersList);
        binaryParseTree.parse(line);
//        binaryParseTree.print();
        List<String> stringTokens = binaryParseTree.listFrominOrderTraversal();
//        Utils.printStringList(stringTokens);
        return stringTokens;

    }

    public List<List<String>> readFileAsBasicStringTokens(File sourceFile) {
        try (BufferedReader reader = new BufferedReader(new FileReader(sourceFile))) {
            return readFromBufferAsBasicTokens(reader);
        } catch (IOException e) {
            throw new Vikari_IOException("Error reading source file: ``" +
                    sourceFile.getName() + "``.");
        }
    }


    /**
     * Analyze the input string as a list of statements. Each analyzed statement is
     * itself returned as a list of string tokens.
     * @param sourceString The input string to analyze
     * @return  A list of list of string tokens.
     */
    public List<List<String>> readStringAsBasicStringTokens(String sourceString) {
        try (BufferedReader reader = new BufferedReader(new StringReader(sourceString))) {
            return readFromBufferAsBasicTokens(reader);
        } catch (IOException e) {
            throw new Vikari_IOException("Error reading source string:\n" +
                    "``" + sourceString + "``.");
        }
    }

    /**
     * Analyzes the input stream targeted by the BufferedReader into a
     * list of list of string tokens.
     * @param reader The BufferedReader to read input from.
     * @return A list of list of srings.
     * @throws IOException If an IO error occurs.
     */
    private List<List<String>> readFromBufferAsBasicTokens(BufferedReader reader) throws IOException {
        List<List<String>> statementsAsStringTokens = new ArrayList<>();
        int lineNumber = 0;
        while (reader.ready()) {
            String line = reader.readLine();
            if (line == null) {
                break;
            }
            System.out.println("line " + lineNumber + ": \"" + line + "\"");

            List<String> stringTokens = tokenize(line);

            System.out.print("tokens: ");
            Utils.printStringList(stringTokens);
            System.out.println();

            lineNumber++;

            statementsAsStringTokens.add(stringTokens);
        }
        return statementsAsStringTokens;
    }

    public List<List<String>> collapseEnclosuresOfStringTokens(List<List<String>> statementsAsStringTokens) {
        System.out.println("--------------------------------------------------------");
        System.out.println("Collapse string tokens.");
        System.out.println("--------------------------------------------------------");


        // ---------------------
        // 1: Collapse comments.
        // ---------------------
        int numberOfLines = statementsAsStringTokens.size();

        String commentPrefix = DefaultIdentifierMapping.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        String commentSuffix = DefaultIdentifierMapping.COMMENT_SUFFIX_CRYSTAL.getIdentifier();
        String leftCurlyBracket = DefaultIdentifierMapping.LEFT_CURLY_BRACKET.getIdentifier();
        String rightCurlyBracket = DefaultIdentifierMapping.RIGHT_CURLY_BRACKET.getIdentifier();
        String backtickQuotation = DefaultIdentifierMapping.BACKTICK.getIdentifier();
        String captureQuotation = DefaultIdentifierMapping.CAPTURE_QUOTATION.getIdentifier();

        List<List<String>> collapsedLines = new ArrayList<>();
        for (int lineNumber = 0; lineNumber <  numberOfLines; lineNumber++) {
            List<String> line = statementsAsStringTokens.get(0);
            List<String> collapsedLine = new ArrayList<>();

            String nextToken;
            for (int tokenNumber = 0; tokenNumber < line.size(); tokenNumber++) {
                String token = line.get(tokenNumber);
                System.out.println("tokenNumber: " + tokenNumber);
                System.out.println("token: " + token);
                System.out.println("[COLLAPSE]");

                // ----------------------
                // 1: Collapse comments.
                // ----------------------
                if (token.equals(commentPrefix)) {
                    CollapseEnclosureResult result = collapseEnclosure(commentPrefix, commentSuffix, tokenNumber, line);
                    tokenNumber = result.columnIndex;
                    nextToken = result.collapsedString;
                    boolean finished = result.finished;
                    while (!finished) {
                        collapsedLine.add(nextToken);
                        collapsedLines.add(collapsedLine);
                        collapsedLine = new ArrayList<>();
                        tokenNumber = 0;
                        lineNumber++;
                        line = statementsAsStringTokens.get(lineNumber);
                        result = collapseEnclosure(null, commentSuffix, tokenNumber, line);

                        tokenNumber = result.columnIndex;
                        nextToken = result.collapsedString;
                        finished = result.finished;
                    }
//                }

//                // collapse Janspirical crystals
//                else if (token.equals(leftCurlyBracket)) {
//                    Pair<Integer, String> comment = collapseEnclosure(leftCurlyBracket, rightCurlyBracket, tokenNumber, line);
//                    tokenNumber = comment.getKey();
//                    nextToken = comment.getValue();
//                }
//
//                // collapse Rapnirical crystals
//                else if (token.equals(rightCurlyBracket)) {
//                    Pair<Integer, String> comment = collapseEnclosure(rightCurlyBracket, leftCurlyBracket, tokenNumber, line);
//                    tokenNumber = comment.getKey();
//                    nextToken = comment.getValue();
//                }
//
//                // collapse capture quotations
//                else if (token.equals(captureQuotation)) {
//                    Pair<Integer, String> comment = collapseEnclosure(captureQuotation, captureQuotation, tokenNumber, line);
//                    tokenNumber = comment.getKey();
//                    nextToken = comment.getValue();
//                }
//
//                // collapse backtick quotations
//                else if (token.equals(backtickQuotation)) {
//                    Pair<Integer, String> comment = collapseEnclosure(backtickQuotation, backtickQuotation, tokenNumber, line);
//                    tokenNumber = comment.getKey();
//                    nextToken = comment.getValue();
                } else {
                    nextToken = token;
                }
                collapsedLine.add(nextToken);

                System.out.println("tokenNumber: " + tokenNumber);
                System.out.println("nextToken: " + nextToken);
                System.out.println("-------------------------------------");

            }
            collapsedLines.add(collapsedLine);
        }


        // 2: Collapse capture quotations.
        // 3: Collapse backtick quotations.
        // 4: Collapse RapniricalCrystalQuotations.
        // 5: Collapse RapniricalCrystalQuotations.
        // 6: Collapse line continuations.
        // 7: collapse whitespace.
        return collapsedLines;
    }

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
     * Collapses an enclosure of String Tokens based on the start and end strings provided.
     * @param startOfEnclosure The beginning of the enclosure.
     * @param endOfEnclosure The ending of the enclosure.
     * @param startIndex The index of what string token to begin collapsing at.
     * @param rowOfStringTokens The current row of string tokens being collapsed.
     * @return A CollapseEnclosureResult holding all data of the result of collapsing this row.
     */
    public CollapseEnclosureResult collapseEnclosure(String startOfEnclosure, String endOfEnclosure, int startIndex, List<String> rowOfStringTokens) {
        StringBuilder sb = new StringBuilder();
        if (startOfEnclosure != null) {
            sb.append(startOfEnclosure);
            startIndex++;
        }

        String nextToken = null;
        for (int i = startIndex; i < rowOfStringTokens.size() && !endOfEnclosure.equals(nextToken); i++) {
            nextToken = rowOfStringTokens.get(i);
            sb.append(nextToken);
            startIndex = i;
        }
//
        int columnIndex = startIndex;
        String collapsedString = sb.toString();
        boolean finished = endOfEnclosure.equals(nextToken);
        return new CollapseEnclosureResult(columnIndex, collapsedString, finished);
    }
}
