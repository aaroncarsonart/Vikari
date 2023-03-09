package com.atonement.crystals.dnr.vikari.interpreter;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.AtonementField;
import com.atonement.crystals.dnr.vikari.core.Statement;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;
import com.atonement.crystals.dnr.vikari.core.separator.WhitespaceCrystal;
import com.atonement.crystals.dnr.vikari.error.Vikari_IOException;
import com.atonement.crystals.dnr.vikari.error.Vikari_LexerException;
import com.atonement.crystals.dnr.vikari.util.Utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class FileInterpreter {

    private Lexer lexer = new Lexer();
    private Parser parser = new Parser();
    private ParseTreeEvaluator evaluator = new ParseTreeEvaluator();

    private List<AtonementCrystal> loadedTypes;

    public FileInterpreter() {
    }

    public AtonementCrystal interpretFile(File sourceFile) {
        List<Statement> statemements = lexer.analyzeAtonementCrystalDefinitionFile(sourceFile);
//        AtonementCrystal = parser.parse(statements);
        return null;
    }


}
