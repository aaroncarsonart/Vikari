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
     * Execute a series of statements, asserting no Exceptions are thrown.
     * @param sourceString The source string to execute.
     */
    public void testVikariStatements(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();
        TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

        SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lexVikariSourceCode(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        assertNoSyntaxErrors(syntaxErrorReporter);

        interpreter.interpret(null, parsedStatements);
    }

    @Test
    @Order(1)
    public void testEmptyStatement() {
        testVikariStatements("");
    }

    @Test
    @Order(2)
    public void testEmptyStatement_WithWhitespace() {
        testVikariStatements("    \t\t    ");
    }

    @Test
    @Order(3)
    public void testMultipleEmptyStatements() {
        testVikariStatements("\n\n\n");
    }

    @Test
    @Order(4)
    public void testMultipleEmptyStatements_WithWhitespace() {
        testVikariStatements("    \n    \n\t\t\n\t\t");
    }

    @Test
    @Order(5)
    public void testEmptyStatements_BetweenExpressionStatements() {
        testVikariStatements("5 + 7\n" +
                             "\n" +
                             "-[22 / 7]");
    }

    @Test
    @Order(6)
    public void testEmptyStatements_BetweenPrintStatements() {
        testVikariStatements(":7:\n" +
                             "\n" +
                             ":22:");
    }

    @Test
    @Order(7)
    public void testEmptyStatements_Complex() {
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
