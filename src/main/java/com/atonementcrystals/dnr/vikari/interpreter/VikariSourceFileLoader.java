package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.error.Vikari_Exception;
import com.atonementcrystals.dnr.vikari.error.Vikari_IOException;
import org.apache.commons.io.FilenameUtils;

import java.io.File;

/**
 * Handles the loading of File objects for files containing Vikari source code.
 */
public class VikariSourceFileLoader {

    /**
     * Load a Vikari source file from the input path or type name.
     * @param sourceFilePathOrTypeName The source file path or type name to load.
     * @return A new File object for the requested source file or type name.
     */
    public File loadSourceFile(String sourceFilePathOrTypeName) {
        try {
            File sourceFile;
            // If the filename ends in .dnr or .DNR, then it is a valid DNR filename.
            if (sourceFilePathOrTypeName.endsWith(".dnr") || sourceFilePathOrTypeName.endsWith(".DNR")) {
                if (fileExists(sourceFilePathOrTypeName)) {
                    sourceFile = new File(sourceFilePathOrTypeName);
                } else {
                    throw new Vikari_IOException("File does not exist: ``" + sourceFilePathOrTypeName + "``." +
                            "\nNote: valid file extensions are: ``.DNR`` and ``.dnr``.");
                }
            }

            // Ensure that all other file extensions are ignored.
            // Files without extensions are already ignored because of how raw type names are evaluated.
            else if (!FilenameUtils.getExtension(sourceFilePathOrTypeName).isEmpty()) {
                throw new Vikari_IOException("File does not exist (2): ``" + sourceFilePathOrTypeName + "``." +
                        "\nNote: valid file extensions are: ``.DNR`` and ``.dnr``.");
            }

            // Otherwise, it must be a type name.
            else {
                sourceFile = resolveFileFromTypeOrScriptName(sourceFilePathOrTypeName);
            }

            return sourceFile;

        } catch (Vikari_Exception e) {
            System.out.println(e.getErrorName() + ": " + e.getErrorMessage());
            System.exit(1);
        }

        throw new IllegalStateException("Unreadable code.");
    }

    /**
     * Get the File for the directory containing the input File.
     * @param file The file to get the directory for.
     * @return The File for the directory containing the input File.
     */
    public File getFileDirectory(File file) {
        return file.getAbsoluteFile().getParentFile();
    }

    /**
     * If a type definition or script source file exists in the local directory, load that file.
     * @param typeOrScriptName The type or script name to load.
     * @return The source file resolved from teh provided type or script name.
     */
    private File resolveFileFromTypeOrScriptName(String typeOrScriptName) {
        if (fileExists(typeOrScriptName + ".DNR")) {
            File crystalDefinitionFile = new File(typeOrScriptName + ".DNR");
            return crystalDefinitionFile;
        }
        if (fileExists(typeOrScriptName + ".dnr")) {
            File scriptFile = new File(typeOrScriptName + ".dnr");
            return scriptFile;
        }

        throw new Vikari_IOException("Type ``" + typeOrScriptName + "`` not resolvable to a Vikari source file." +
                "\nNote: valid file extensions are: ``.DNR`` and ``.dnr``.");
    }

    /**
     * Provides a case-sensitive check for if a provided filename exists.
     */
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

}
