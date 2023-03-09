package com.atonement.crystals.dnr.vikari.util;

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
}
