package com.atonement.crystals.dnr.vikari.util;

import com.atonement.crystals.dnr.vikari.error.Vikari_LexerException;

import java.util.List;

public class Utils {

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

    public static boolean isLongNumber(String string) {
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

    public static String stripEnclosure(String enclosedString, String startEnclosure, String endEnclosure) {
        if (!enclosedString.startsWith(startEnclosure)) {
            throw new Vikari_LexerException("String missing start enclosure: " + startEnclosure);
        }
        if (!enclosedString.endsWith(endEnclosure)) {
            throw new Vikari_LexerException("String missing end enclosure: " + startEnclosure);
        }
        int startIndex = startEnclosure.length();
        int endIndex = enclosedString.length() - endEnclosure.length();
        return enclosedString.substring(startIndex, endIndex);
    }

    public static boolean isWhitespace(String identifier) {
        return identifier != null && identifier.matches("[ \\t]+");
    }
}
