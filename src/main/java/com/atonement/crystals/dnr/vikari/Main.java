package com.atonement.crystals.dnr.vikari;

import com.atonement.crystals.dnr.vikari.interpreter.VikariProgram;
import com.atonement.crystals.dnr.vikari.interpreter.VikariSourceFileLoader;

import java.io.File;

/**
 * TODO: Make the program accept a number of options in the arguments.<br/>
 * TODO: See https://opensource.com/article/21/8/java-commons-cli.
 * <br/>
 * Program options:<br/>
 * <br/>
 * -l [--lex] executes the lexer, and prints the scanned tokens.<br/>
 * -p [--parse] executes the parser, and prints the resulting AST.<br/>
 * -e [-execute] executes the interpreter, and runs the Vikari program.<br/>
 * -f [--file <path_to_file>] accepts a path to a Vikari source file.<br/>
 * -T [--type <type_name>] accepts a qualified type name: ``example::test::ExampleCrystal``<br/>
 * -c [--code <source_code>] accepts a string containing Vikari source code.<br/>
 * -l [--line-numbers] Prepends the line number before each lexed statement.<br/>
 * -i [---show-invisibles] shows invisible characters with "·", "→", and "¶" in lexer and parser output.<br/>
 * -t [--print-tokens] prints each token as comma-separated strings in the lexer output.<br/>
 * -v [--verbose] prints each token as comma-separated strings wrapped with their type names in the lexer output.<br/>
 * <br/>
 * Default behavior is to accept a type name OR a path to a file and then execute it.
 */
public class Main {
    public static void main(String[] args) {
        runVikariSourceFile(args);
    }

    public static void runVikariSourceFile(String[] args) {
        if (args.length != 1) {
            System.out.println("IO Error: accepts one argument with format:" +
                    "\n    ``Type``" +
                    "\n    ``path/to/Type.DNR``" +
                    "\n    ``path/to/script.dnr``" +
                    "\n" +
                    "\n(case-sensitive)");
            System.exit(0);
        }
        String vikariFileOrTypeName = args[0];

        VikariSourceFileLoader sourceFileLoader = new VikariSourceFileLoader();
        File rootSourceFile = sourceFileLoader.loadSourceFile(vikariFileOrTypeName);

        VikariProgram program = new VikariProgram();
        program.lexAndParse(rootSourceFile);
        program.execute();
    }
}
