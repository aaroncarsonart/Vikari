package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.Statement;

import java.io.File;
import java.util.List;

public class FileInterpreter {

    private Lexer lexer = new Lexer();
    private Parser parser = new Parser();
    private ParseTreeEvaluator evaluator = new ParseTreeEvaluator();

    private List<AtonementCrystal> loadedTypes;

    public FileInterpreter() {
    }

    public AtonementCrystal interpretFile(File sourceFile) {
        // TODO Finish implementation. Correct type of statements variable.
        List<Statement> statements = lexer.analyzeAtonementCrystalDefinitionFile(sourceFile);
        return null;
    }


}
