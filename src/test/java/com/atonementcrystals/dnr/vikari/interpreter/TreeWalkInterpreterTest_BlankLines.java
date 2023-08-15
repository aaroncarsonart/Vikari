package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoSyntaxErrors;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_BlankLines {

    /**
     * Execute a series of statements, asserting no SyntaxErrors are reported.
     * @param sourceString The source string to execute.
     */
    public void testVikariStatements(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);
        interpreter.setGetLineFunction(syntaxErrorReporter::getLineFromCache);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        interpreter.interpret(null, parsedStatements);
    }

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_EmptyStatement() {
        testVikariStatements("");
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_EmptyStatement_WithWhitespace() {
        testVikariStatements("    \t\t    ");
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_MultipleEmptyStatements() {
        testVikariStatements("\n\n\n");
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_MultipleEmptyStatements_WithWhitespace() {
        testVikariStatements("    \n    \n\t\t\n\t\t");
    }

    @Test
    @Order(5)
    public void testTreeWalkInterpreter_EmptyStatements_BetweenExpressionStatements() {
        testVikariStatements("5 + 7\n" +
                             "\n" +
                             "-[22 / 7]");
    }

    @Test
    @Order(6)
    public void testTreeWalkInterpreter_EmptyStatements_BetweenPrintStatements() {
        testVikariStatements(":7:\n" +
                             "\n" +
                             ":22:");
    }

    @Test
    @Order(7)
    public void testTreeWalkInterpreter_EmptyStatements_Complex() {
        String sourceString = ":22 + 2\n" +
                "\n" +
                "\n" +
                ":7\n" +
                ":3\n" +
                ":\n" +
                "\n" +
                ":-[127.3B - 77F] / 22.33D:" +
                "\n" +
                "\n";
        testVikariStatements(sourceString);
    }
}
