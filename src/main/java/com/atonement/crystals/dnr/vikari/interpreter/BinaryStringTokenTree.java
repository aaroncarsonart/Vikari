package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Splits a single line of Vikari source code into tokens. The output
 * of traversing this tree requires collapsion before being passed
 * to the final phase of the Lexer.
 */
public class BinaryStringTokenTree {

    private static final List<String> tokenRegexList = initializeTokenRegexList();

    /**
     * Generates the list of regular expressions upon which to tokenize
     * a single line of Vikari source code.
     *
     * @return A list of regular expressions.
     */
    private static List<String> initializeTokenRegexList() {
        List<String> tokenRegexList = TokenType.LEXER_TOKENS.stream()
                .map(TokenType::getIdentifier)
                .distinct()
                .sorted(Comparator.comparingInt(String::length).reversed())
                .map(Pattern::quote)
                .collect(Collectors.toCollection(ArrayList::new));

        // Match a word beginning with "vv" that isn't preceeded by a non-digit word character.
        String breakTokenRegex = "(?<!\\w)vv(?![A-Za-z_])";
        tokenRegexList.add(breakTokenRegex);

        // TODO: Support regex to collapse whitespaces without needing to stitch every individual space and tab
        // TODO: together afterwards.

        // split on whitespace first
        tokenRegexList.add(0, " ");
        tokenRegexList.add(1, "\t");

        return tokenRegexList;
    }

    /**
     * Represents a single node in the tree.
     */
    class BinaryStringTokenTreeNode {
        String token;
        BinaryStringTokenTreeNode left;
        BinaryStringTokenTreeNode right;
        BinaryStringTokenTreeNode(String token) {
            this.token = token;
        }
    }

    private BinaryStringTokenTreeNode rootNode;

    public BinaryStringTokenTree(String text) {
        rootNode = splitAndAddNode(text);
    }

    /**
     * Splits a string on the first matched regular expression. Extracts
     * the token, places that in a tree node, and then recursively
     * tokenizes the left and right portions of the remaining string.
     *
     * @param line The line (or substring or a line) to tokenize.
     * @return The tree node resulting from recursively splitting the
     *         line on regular expressions in tokenRegexList.
     */
    private BinaryStringTokenTreeNode splitAndAddNode(String line) {
        for (String regex : tokenRegexList) {
            Pattern pattern = Pattern.compile(regex);
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                String token = matcher.group();
                BinaryStringTokenTreeNode node = new BinaryStringTokenTreeNode(token);
                int start = matcher.start();
                int end = matcher.end();
                String left = line.substring(0, start);
                String right = line.substring(end);
                if (!left.isEmpty()) {
                    node.left = splitAndAddNode(left);
                }
                if (!right.isEmpty()) {
                    node.right = splitAndAddNode(right);
                }
                return node;
            }
        }
        return new BinaryStringTokenTreeNode(line);
    }

    /**
     * Print the tree. (For debugging.)
     */
    public void print() {
        printBinaryParseTree(rootNode);
    }

    /**
     * Recursively print the parse tree by performing an in-order traversal.
     * @param treeNode The tree node to begin the printing at.
     */
    private void printBinaryParseTree(BinaryStringTokenTreeNode treeNode) {
        if (treeNode != null) {
            printBinaryParseTree(treeNode.left);
            System.out.println(treeNode.token);
            printBinaryParseTree(treeNode.right);
        }
    }

    /**
     * Extract a sequential list of string tokens by performing an in-order
     * traversal of this tree.
     * @return The list of string tokens generated by this tree.
     */
    public List<String> listFrominOrderTraversal() {
        List<String> tokensList = new ArrayList<>();
        listFrominOrderTraversal(rootNode, tokensList);
        return tokensList;
    }

    /**
     * Recursively extract a sequential list of string tokens by performing an
     * in-order traversal of this tree.
     * @return The list of string tokens generated by this tree.
     */
    private void listFrominOrderTraversal(BinaryStringTokenTreeNode treeNode, List<String> tokensList) {
        if (treeNode.left != null) {
            listFrominOrderTraversal(treeNode.left, tokensList);
        }
        tokensList.add(treeNode.token);
        if (treeNode.right != null) {
            listFrominOrderTraversal(treeNode.right, tokensList);
        }
    }
}
