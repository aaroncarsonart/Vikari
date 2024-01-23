package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;

import java.util.List;

import static org.junit.jupiter.api.Assertions.fail;

public class TreeWalkInterpreterPrintTest_Base extends PrintTest_Base {
    protected final AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();

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

        if (syntaxErrorReporter.hasErrors()) {
            restorePrintStream();
            syntaxErrorReporter.reportSyntaxErrors();
            fail("Expected no syntax errors for test case.");
        }

        interpreter.interpret(null, parsedStatements);
    }

    /**
     * Tests a boolean expression by prepending a print statement operator ":"
     * and then comparing it against the string value of the boolean expectedValue.
     * @param sourceString The boolean expression to test.
     * @param expectedValue The boolean value to expect the expression to return.
     */
    public void testBooleanExpression(String sourceString, boolean expectedValue) {
        String finalSourceString = ":" + sourceString;
        lexParseAndInterpret(finalSourceString);

        String expectedOutput = String.valueOf(expectedValue);
        testOutput(expectedOutput);
    }
}
