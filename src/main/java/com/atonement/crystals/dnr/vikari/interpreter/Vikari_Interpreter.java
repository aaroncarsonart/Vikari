package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.error.Vikari_Error;
import com.atonement.crystals.dnr.vikari.error.Vikari_IOException;
import com.atonement.crystals.dnr.vikari.error.Vikari_SyntaxErrorException;
import org.apache.commons.io.FilenameUtils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class Vikari_Interpreter {

    private File rootFile;
    private File rootDirectory;

    // NOTE: if you load the interpreter without a file,
    // then you enter repl mode.
    // This will be implemented at a later time.
    public Vikari_Interpreter() {
    }

    // The call structure of the interpreter should be as follows:
    // dnr RootCrystal
    // dnr path/to/RootCrystal.DNR

    // So, if a DNR file exists with the given Type name in the present working directory,
    // that file will then be loaded and immediately executed.

    // Otherwise, the input string will be interpreted as a path to a file,
    // which will then be loaded and executed.

    // In either case, the program's root directory will be set based on the root crystal.

    // Lowercase is used for the filename and extension if the file is simply a script:
    // my_dnr_script.dnr

    // PascalCase is used for the filename and uppercase for the extension if the file is a
    // formal AtonementCrystal definition:
    // MyDnrCrystalDefinition.DNR

    public Vikari_Interpreter(String dnrFileOrTypeName) {
        try {
            // If the filename ends in .dnr or .DNR, then it is a valid DNR filename.
            if (dnrFileOrTypeName.endsWith(".dnr") || dnrFileOrTypeName.endsWith(".DNR")) {                 if (fileExists(dnrFileOrTypeName)) {
                    rootFile = new File(dnrFileOrTypeName);
                } else {
                    throw new Vikari_IOException("DNR file does not exist: ``" + dnrFileOrTypeName + "``." +
                            "\nNote: valid DNR file extensions are: ``.DNR`` and ``.dnr``.");
                }
            }

            // Ensure that all other file extensions are ignored.
            // Files without extensions are already ignored because of how raw type names are evaluated.
            else if (!FilenameUtils.getExtension(dnrFileOrTypeName).isEmpty()) {
                throw new Vikari_IOException("DNR file does not exist (2): ``" + dnrFileOrTypeName + "``." +
                        "\nNote: valid DNR file extensions are: ``.DNR`` and ``.dnr``.");
            }

            // Otherwise, it must be a type name.
            else {
                rootFile = resolveFileFromTypeName(dnrFileOrTypeName);
            }

            rootDirectory = rootFile.getAbsoluteFile().getParentFile();
            System.out.println("rootFile: " + rootFile);
            System.out.println("rootDirectory: " + rootDirectory);
        } catch (Vikari_Error e) {
            System.out.println(e.getErrorName() + ": " + e.getErrorMessage());
            System.exit(1);
        }
    }

    private File resolveFileFromTypeName(String typename) {
        if (fileExists(typename + ".DNR")) {
            File crystalDefinitionFile = new File (typename + ".DNR");
            return crystalDefinitionFile;
        }
        if (fileExists(typename + ".dnr")) {
            File scriptFile = new File (typename + ".dnr");
            return scriptFile;
        }

        throw new Vikari_IOException("Type ``" + typename + "`` not resolvable to a DNR file." +
                "\nNote: valid DNR file extensions are: ``.DNR`` and ``.dnr``.");
    }

    // case-sensitive filename checker
    private boolean fileExists(String filename) {
        File maybeFile = new File(filename);
        String baseFilename = maybeFile.getName();
        File directory = maybeFile.getAbsoluteFile().getParentFile();
        String [] directoryContents = directory.list();
        if (directoryContents != null) {
            for (String file : directoryContents) {
                if (file.equals(baseFilename)) {
                    if (maybeFile.isFile() && !maybeFile.isDirectory()) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    // Simple printing of a file's contents.
    // For debugging purposes.
    private void readFile(File file) {
        try (BufferedReader bufferedReader = new BufferedReader(new FileReader(file))) {
            String next;
            while ((next = bufferedReader.readLine()) != null) {
                System.out.println(next);
            }

        } catch (IOException e) {
            throw new Vikari_IOException(e.getMessage());
        }
    }

    // TODO: Function to parse a fully-qualified type name into a relative file path
    // TODO: based off of the project's root folder.

    // Function to validate strings as fully-qualified type names
    // complete with packages delimited by cons terminated by a type name.
    // Untested.

    public boolean validateFullyQualifiedTypeName(String fullyQualifiedTypeName) {
        fullyQualifiedTypeName = fullyQualifiedTypeName.trim();
        String[] tokens = fullyQualifiedTypeName.split("::");
        int length = tokens.length;

        // ensure first and last tokens are not empty
        if (tokens[0].isEmpty()) {
            throw new Vikari_SyntaxErrorException("Fully-qualified Type names cannot begin with `::`.");
        }

        if (tokens[length - 1].isEmpty()) {
            throw new Vikari_SyntaxErrorException("Missing type name after `::`.");
        }

        // Packages and type names follow the same naming conventions.
        // Upper and lowercase letters and numbers only.
        // The convention is that packages are always lowercase,
        // While type names are always capitalized.
        // Unless a file is a simple script,
        // in which case the type name should be in lowercase.

        for (int i = 0; i < length; i++) {
            String token = tokens[i];
            if (!token.matches("[A-Za-z0-9_-]+")) {
                boolean isPackage = i < length - 1;
                if (isPackage) {
                    throw new Vikari_SyntaxErrorException("Invalid character(s) in package name: `" + token + "`. " +
                            "Packages may only contain letters, numbers, underscores, and dashes.");
                } else {
                    throw new Vikari_SyntaxErrorException("Invalid character(s) in Type name: `" + token + "`. " +
                            "Types may only contain letters, numbers, underscores, and dashes.");
                }
            }
        }

        return true;
    }

    // Sequentially executes all statements in the root crystal's definition file.
    public void execute() {
    }
}
