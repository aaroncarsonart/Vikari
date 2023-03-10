package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Splits an input string according to all default identifiers that won't
 * match against ordinary unquoted identifiers.
 */
public class BinaryParseTree {

    class BinaryParseTreeNode {
        String token;
        BinaryParseTreeNode left;
        BinaryParseTreeNode right;
        BinaryParseTreeNode(String token) {
            this.token = token;
        }
    }

    private List<String> defaultIdentifiersList;
    private BinaryParseTreeNode rootNode;

    public BinaryParseTree(List<String> defaultIdentifiersList) {
        defaultIdentifiersList.sort(Comparator.comparingInt(String::length).reversed());

        // split on whitespace first
        defaultIdentifiersList.add(0, " ");
        defaultIdentifiersList.add(1, "\t");

        // remove keywords that would clash with other identifiers
        defaultIdentifiersList.remove(DefaultIdentifierMapping.PACKAGE.getIdentifier());
        defaultIdentifiersList.remove(DefaultIdentifierMapping.IMPORT.getIdentifier());
        defaultIdentifiersList.remove(DefaultIdentifierMapping.PUBLIC_ACCESS_MODIFIER.getIdentifier());
        defaultIdentifiersList.remove(DefaultIdentifierMapping.PRIVATE_ACCESS_MODIFIER.getIdentifier());
        defaultIdentifiersList.remove(DefaultIdentifierMapping.TRUE.getIdentifier());
        defaultIdentifiersList.remove(DefaultIdentifierMapping.FALSE.getIdentifier());
        defaultIdentifiersList.remove(DefaultIdentifierMapping.SWORD.getIdentifier());

        this.defaultIdentifiersList = defaultIdentifiersList;
    }

    public void parse(String text) {
        rootNode = splitAndAddNode(text);
    }

    private BinaryParseTreeNode splitAndAddNode(String text) {
        for (String identifier : defaultIdentifiersList) {
            String[] strings = text.split(Pattern.quote(identifier), 2);
            if (strings.length == 2) {
                BinaryParseTreeNode node = new BinaryParseTreeNode(identifier);
                String left = strings[0];
                String right = strings[1];
                if (!left.isEmpty()) {
                    node.left = splitAndAddNode(left);
                }
                if (!right.isEmpty()) {
                    node.right = splitAndAddNode(right);
                }
                return node;
            }
        }
        return new BinaryParseTreeNode(text);
    }

    public void print() {
        printBinaryParseTree(rootNode);
    }

    private void printBinaryParseTree(BinaryParseTreeNode treeNode) {
        if (treeNode != null) {
            printBinaryParseTree(treeNode.left);
            System.out.println(treeNode.token);
            printBinaryParseTree(treeNode.right);
        }
    }

    public List<String> listFrominOrderTraversal() {
        List<String> tokensList = new ArrayList<>();
        listFrominOrderTraversal(rootNode, tokensList);
        return tokensList;
    }

    private void listFrominOrderTraversal(BinaryParseTreeNode treeNode, List<String> tokensList) {
        if (treeNode.left != null) {
            listFrominOrderTraversal(treeNode.left, tokensList);
        }
        tokensList.add(treeNode.token);
        if (treeNode.right != null) {
            listFrominOrderTraversal(treeNode.right, tokensList);
        }
    }
}
