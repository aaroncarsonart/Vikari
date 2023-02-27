package com.atonement.crystals.dnr.vikari;

import com.atonement.crystals.dnr.vikari.interpreter.Vikari_Interpreter;

public class Main {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("IO Error: accepts one argument with format:" +
                    "\n    ``Type``" +
                    "\n    ``Type.DNR``." +
                    "\n" +
                    "\n(case-sensitive)");
            System.exit(0);
        }
        String dnrFileOrPathName = args[0];
        Vikari_Interpreter interpreter = new Vikari_Interpreter(dnrFileOrPathName);
        interpreter.execute();
    }
}
