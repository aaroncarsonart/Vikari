package com.atonementcrystals.dnr.vikari.util;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.Keyword;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;
import com.atonementcrystals.dnr.vikari.error.Vikari_LexerException;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.FileSystems;
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
    private static String userDir = System.getProperty("user.dir");

    /**
     * Regular expression pattern for type names.
     */
    private static final Pattern typeIdentifierRegex = Pattern.compile("[A-Z][A-Za-z0-9_]*");

    /**
     * Regular expression pattern for field region names.
     */
    private static final Pattern fieldRegionIdentifierRegex = Pattern.compile("[a-z_][a-z0-9_]*");

    /**
     * Regular expression pattern for crystal reference names. This pattern doesn't detect error
     * cases for backtick quoted identifiers, like all whitespace or if it contains tabs.
     */
    private static final Pattern crystalIdentifierRegex = Pattern.compile("(?:[a-z_][A-Za-z0-9_]*|(?<!`.)`[^`\\n]{2,}`)");

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
        for (int i = 0; i < length; i++) {
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

    public static boolean isIntegerNumber(String string) {
        try {
            Integer.valueOf(string);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    public static boolean isLongIntegerNumber(String string) {
        try {
            String value = string;
            if (Utils.hasLongSuffix(value)) {
                value = Utils.trimLastCharacter(value);
            }
            Long.valueOf(value);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }

    /**
     * Test if an identifier ends with "l" or "L".
     * @param string The identifier to test.
     * @return True if the identifier has a Long suffix.
     */
    public static boolean hasLongSuffix(String string) {
        if (string.endsWith("l") || string.endsWith("L")) {
            return true;
        }
        return false;
    }

    /**
     * Test if an identifier ends with "b" or "B".
     * @param string The identifier to test.
     * @return True if the identifier has a Big suffix.
     */
    public static boolean hasBigSuffix(String string) {
        if (string.endsWith("b") || string.endsWith("B")) {
            return true;
        }
        return false;
    }

    /**
     * Trims the last character in the string. Assumes a previous check to hasLongSuffix()
     * or hasBigSuffix() has returned true for the input string.
     * @param string The string to trim.
     * @return A string with the last character trimmed.
     */
    public static String trimLastCharacter(String string) {
        return string.substring(0, string.length() - 1);
    }

    public static boolean isBigIntegerNumber(String string) {
        try {
            String value = string;
            if (Utils.hasBigSuffix(value)) {
                value = Utils.trimLastCharacter(value);
            }
            new BigInteger(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static boolean isFloatNumber(String string) {
        try {
            // Don't allow explicit Double literals.
            if (string.endsWith("d") || string.endsWith("D")) {
                return false;
            }
            Float.valueOf(string);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static boolean isDoubleNumber(String string) {
        try {
            // Don't allow explicit Float literals.
            if (string.endsWith("f") || string.endsWith("F")) {
                return false;
            }
            Double.valueOf(string);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static boolean isBigDecimalNumber(String string) {
        try {
            String value = string;
            if (Utils.hasBigSuffix(value)) {
                value = Utils.trimLastCharacter(value);
            }
            new BigDecimal(value, Arithmetic.getMathContext());
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    /**
     * Checks if the string is an integer number followed by f, F, d, D, b, or B.
     * @param string The identifier to check.
     * @return True if the string is a valid fractional part of a decimal number; else false.
     */
    public static boolean isValidDecimalFractionalPart(String string) {
        // 1. Check suffix.
        String suffix = string.substring(string.length() - 1);
        String allowedSuffixes = "fFdDbB";
        boolean hasSuffix = allowedSuffixes.contains(suffix);

        // 2. Get number part.
        String number;
        if (hasSuffix) {
            number = string.substring(0, string.length() - 1);
        } else {
            number = string;
        }

        // 3. Check number.
        try {
            new BigInteger(number);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    public static boolean isEnclosedString(String string, String leftEnclosure, String rightEnclosure) {
        if (string == null) {
            return false;
        }
        String regex = java.lang.String.format("^\\Q%s\\E.*\\Q%s\\E$", leftEnclosure, rightEnclosure);
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(string);
        return matcher.find();
    }

    public static boolean isStringLiteral(String string) {
        String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
        if (isEnclosedString(string, captureQuotation, captureQuotation)) {
            int closingTokenIndex = string.length() - captureQuotation.length();
            return !isEscapedByBackslash(string, closingTokenIndex);
        }
        return false;
    }

    public static boolean isStartOfStringLiteral(String string) {
        String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
        return string != null && string.startsWith(captureQuotation);
    }

    public static boolean isEndOfStringLiteral(String string) {
        String captureQuotation = TokenType.CAPTURE_QUOTATION.getIdentifier();
        if (string != null && string.endsWith(captureQuotation)) {
            int closingTokenIndex = string.length() - captureQuotation.length();
            return !isEscapedByBackslash(string, closingTokenIndex);
        }
        return false;
    }

    /**
     * Ensure an odd number of backslashes precedes the character marked by the index in the given string.
     * @param text The string to check for backslashes in.
     * @param index The index behind which to count the backslashes.
     * @return True if the character marked by the index is escaped by a backslash.
     */
    public static boolean isEscapedByBackslash(String text, int index) {
        if (text.isEmpty() || index < 0 || index >= text.length()) {
            return false;
        }
        int backslashCount = 0;
        for (int i = index - 1; i >= 0; i--) {
            char prev = text.charAt(i);
            if (prev == '\\') {
                backslashCount++;
            } else {
                break;
            }
        }
        return backslashCount % 2 == 1;
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
        if (string != null && string.endsWith(commentSuffix)) {
            int closingTokenIndex = string.length() - commentSuffix.length();
            return !isEscapedByBackslash(string, closingTokenIndex);
        }
        return false;
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
     * Count the number of occurrences of the searchString in the sourceString.
     * @param sourceString The string to count the number of occurrences in.
     * @param searchString The string the count the number of occurrences for.
     * @param regionEnd The end of the region to search in sourceString.
     * @return The number of occurrences.
     */
    public static int countOccurrences(String sourceString, String searchString, int regionEnd) {
        Pattern pattern = Pattern.compile(Pattern.quote(searchString));
        Matcher matcher = pattern.matcher(sourceString);
        matcher.region(0, regionEnd);
        int matchCount = (int) matcher.results().count();
        return matchCount;
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
        return getSimpleClassName(crystal.getClass());
    }

    /**
     * Strips the word "Crystal" from the end of any AtonementCrystal
     * class name. (Except for the AtonementCrystal class itself.)
     *
     * @param crystalType The AtonementCrystal to simplify the class name for.
     * @return The simplified form of the crystal's class name.
     */
    public static String getSimpleClassName(Class<? extends AtonementCrystal> crystalType) {
        String name = crystalType.getSimpleName();
        if (crystalType == AtonementCrystal.class) {
            return name;
        }
        if (name.endsWith("Crystal")) {
            int end = name.indexOf("Crystal");
            name = name.substring(0, end);
            return name;
        }
        return name;
    }

    public static boolean validateFullyQualifiedTypeName(String fullyQualifiedTypeName) {
        // Null and empty strings are invalid.
        if (fullyQualifiedTypeName == null || fullyQualifiedTypeName.equals("")) {
            return false;
        }

        String[] tokens = fullyQualifiedTypeName.split("::");

        // Strings of only :: operators are invalid.
        if (tokens.length == 0) {
            return false;
        }

        Pattern packageNamePattern = Pattern.compile("^[a-z][a-z0-9_]*$");

        // Validate the package names.
        for (int i = 0; i < tokens.length - 1; i++) {
            String packageToken = tokens[i];
            Matcher matcher = packageNamePattern.matcher(packageToken);
            if (!matcher.find()) {
                return false;
            }
        }

        // Validate the type name.
        int typeTokenIndex = tokens.length == 1 ? 0 : tokens.length - 1;
        String typeToken = tokens[typeTokenIndex];

        // check if final token is a Type name.
        Pattern typeNamePattern = Pattern.compile("^[A-Z]\\w*$");
        Matcher matcher = typeNamePattern.matcher(typeToken);
        if (matcher.find()) {
            return true;
        }

        // check if final token is a script name.
        String scriptToken = typeToken;
        matcher = packageNamePattern.matcher(scriptToken);
        return matcher.find();
    }

    /**
     * Generates a file path for the following fully-qualified type name.
     * Assumes it to be validated by validateFullyQualifiedTypeName().
     * @param fullyQualifiedTypeName The type name to convert into a file path.
     * @return The file path represented by this fully-qualified type name.
     */
    public static String filePathForTypeName(String fullyQualifiedTypeName) {
        String[] tokens = fullyQualifiedTypeName.split("::");
        String fileSeparator = FileSystems.getDefault().getSeparator();

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < tokens.length - 1; i++) {
            String directoryToken = tokens[i];
            sb.append(directoryToken);
            sb.append(fileSeparator);
        }

        String fileToken = tokens[tokens.length - 1];
        sb.append(fileToken);

        char firstCharacter = fileToken.charAt(0);
        boolean capitalized = Character.isUpperCase(firstCharacter);

        // Is a type file.
        if (capitalized) {
            sb.append(".DNR");
        }

        // Is a script file.
        else {
            sb.append(".dnr");
        }

        return sb.toString();
    }

    /**
     * Shorten the filename by truncating it via removing the prefix of the
     * current directory where Vikari is running.
     * @param file The File to shorten the filename for.
     * @return The shortened filename.
     */
    public static String getShortenedFilename(File file) {
        String filename;
        try {
            filename = file.getCanonicalPath();
        } catch (IOException e) {
            filename = file.getAbsolutePath();
        }

        if (filename.startsWith(userDir)) {
            return filename.substring(userDir.length() + 1);
        }
        return filename;
    }

    public static boolean isCrystalIdentifier(String identifier) {
        Matcher matcher = crystalIdentifierRegex.matcher(identifier);
        return matcher.matches();
    }

    public static boolean isFieldRegionIdentifier(String identifier) {
        Matcher matcher = fieldRegionIdentifierRegex.matcher(identifier);
        return matcher.matches();
    }

    public static boolean isTypeIdentifier(String identifier) {
        Matcher matcher = typeIdentifierRegex.matcher(identifier);
        return matcher.matches();
    }
}
