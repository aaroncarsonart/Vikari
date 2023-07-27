package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class TreeWalkInterpreterPrintTest_Base {
    private final PrintStream originalOut = System.out;
    protected final ByteArrayOutputStream testOut = new ByteArrayOutputStream();
    protected final AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();

    @BeforeEach
    public void setupPrintStream() {
        System.setOut(new PrintStream(testOut));
    }

    @AfterEach
    public void restorePrintStream() {
        System.setOut(originalOut);
    }

    public void lexParseAndInterpret(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLineFromCache);

        parser.setGlobalAtonementField(globalAtonementField);
        interpreter.setGlobalAtonementField(globalAtonementField);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);
        assertNoSyntaxErrors(syntaxErrorReporter);
        interpreter.interpret(null, parsedStatements);
    }

    public void testOutput(String expectedOutput) {
        String actualOutput = testOut.toString();
        assertEquals(expectedOutput, actualOutput, "Unexpected output of print statement.");
        testOut.reset();
    }
}
