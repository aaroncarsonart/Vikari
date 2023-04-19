package com.atonement.crystals.dnr.vikari.util;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.Keyword;
import com.atonement.crystals.dnr.vikari.core.identifier.TokenType;
import com.atonement.crystals.dnr.vikari.error.Vikari_LexerException;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Various utility methods for:
 * <ul>
 *     <li>Printing out values.</li>
 *     <li>Checking values of strings.</li>
 *     <li>Modifying string values.</li>
 * </ul>
 */
public class Utils {

    /**
     * Do not instantiate the Utils class.
     */
    private Utils() {
    }

    /**
     * Prints a list of strings as ["str1,"str2"...].
     * Escapes special characters like newlines and tabs.
     *
     * @param list The list of strings to print.
     */
    public static void printStringList(List<String>list) {
        if (list == null) {
            System.out.println("null");
            return;
        }

        System.out.print("[");
            for (int i = 0; i < list.size(); i++) {
                String token = list.get(i);

                System.out.print("\"");

                if (token.equals("\n")) {
                    System.out.print("\\n");
                } else if (token.equals("\t")) {
                    System.out.print("\\t");
                } else if (token.equals("\"")){
                    System.out.print("\\\"");
                } else {
                    System.out.print(token);
                }

                System.out.print("\"");

                if (i < list.size() - 1) {
                    System.out.print(",");
                }
            }
        System.out.println("]");
    }

    /**
     * Prints out a line of characters repeated by the given length.
     *
     * @param c The character to print.
     * @param length The number of times to repeat printing the character.
     */
    public static void printLineOfChars(char c, int length) {
        for (int i = 0; i < length + 8; i++) {
            System.out.print(c);
        }
        System.out.println();
    }

    public static boolean isSword(String string) {
        if (string == null || string.length() == 0) {
            return false;
        }
        String swordIdentifier = TokenType.SWORD.getIdentifier();
        char swordChar = swordIdentifier.charAt(0);
        char[] chars = string.toCharArray();
        for (char c : chars) {
            if (c != swordChar) {
                return false;
            }
        }
        return true;
    }

    public static boolean isBooleanLiteral(String string) {
        String trueLiteral = Keyword.TRUE.getIdentifier();
        String falseLiteral = Keyword.FALSE.getIdentifier();
        return string != null && (string.equals(trueLiteral) || string.equals(falseLiteral));
    }

    public static boolean isLongIntegerNumber(String string) {
        try {
            Long.valueOf(string);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    public static boolean isDecimalNumber(String string) {
        try {
            Double.valueOf(string);
        } catch (NumberFormatException e) {
            return false;
        }
        return string != null && string.contains(".");
    }

    public static boolean isEnclosedString(String string, String leftEnclosure, String rightEnclosure) {
        if (string == null) {
            return false;
        }
        String regex = String.format("^\\Q%s\\E.*\\Q%s\\E$", leftEnclosure, rightEnclosure);
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(string);
        return matcher.find();
    }

    public static boolean isStringLiteral(String string) {
        String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
        return isEnclosedString(string, captureQuotation, captureQuotation);
    }

    public static boolean isStartOfStringLiteral(String string) {
        String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
        return string != null && string.startsWith(captureQuotation);
    }

    public static boolean isEndOfStringLiteral(String string) {
        String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
        return string != null && string.endsWith(captureQuotation);
    }

    public static boolean isBacktickQuotedIdentifier(String string) {
        String backtick = TokenType.BACKTICK.getIdentifier();
        boolean hasCorrectLength = string.length() > 2;
        return hasCorrectLength && isEnclosedString(string, backtick, backtick);
    }

    public static boolean isSingleLineComment(String string) {
        String commentPrefix = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        String commentSuffix = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();
        return isEnclosedString(string, commentPrefix, commentSuffix);
    }

    public static boolean isStartOfComment(String string) {
        String commentPrefix = TokenType.COMMENT_PREFIX_CRYSTAL.getIdentifier();
        return string != null && string.startsWith(commentPrefix);
    }

    public static boolean isEndOfComment(String string) {
        String commentSuffix = TokenType.COMMENT_SUFFIX_CRYSTAL.getIdentifier();
        return string != null && string.endsWith(commentSuffix);
    }

    public static String stripEnclosure(String enclosedString, String startEnclosure, String endEnclosure) {
        if (!enclosedString.startsWith(startEnclosure)) {
            // This is an internal error. Should halt program.
            throw new Vikari_LexerException("Internal error. String missing start enclosure: " + startEnclosure);
        }
        if (!enclosedString.endsWith(endEnclosure)) {
            // This is an internal error. Should halt program.
            throw new Vikari_LexerException("Internal error. String missing end enclosure: " + endEnclosure);
        }
        int startIndex = startEnclosure.length();
        int endIndex = enclosedString.length() - endEnclosure.length();
        return enclosedString.substring(startIndex, endIndex);
    }

    public static boolean isWhitespace(String identifier) {
        return identifier != null && identifier.matches("[ \\t]+");
    }

    /**
     * Replaces spaces, tabs, and newlines with "·", "→", and "¶".
     * @param text The string to modify.
     * @return A string with spaces, tabs, and newlines replaced.
     */
    public static String showInvisibles(String text) {
        char space = ' ';
        char tab = '\t';
        char newline = '\n';

        char visibleSpace = '·';
        char visibleTab = '→';
        char visibleNewline = '¶';

        text = text.replace(space, visibleSpace);
        text = text.replace(tab, visibleTab);
        text = text.replace(newline, visibleNewline);

        return text;
    }

    /**
     * Strips the word "Crystal" from the end of any AtonementCrystal's
     * class name. (Except for the AtonementCrystal class itself.)
     *
     * @param crystal The AtonementCrystal to simplify the class name for.
     * @return The simplified form of the crystal's class name.
     */
    public static String getSimpleClassName(AtonementCrystal crystal) {
        String name = crystal.getClass().getSimpleName();
        if (name.equals(AtonementCrystal.class.getSimpleName())) {
            return name;
        }
        int end = name.indexOf("Crystal");
        name = name.substring(0, end);
        return name;
    }
}
