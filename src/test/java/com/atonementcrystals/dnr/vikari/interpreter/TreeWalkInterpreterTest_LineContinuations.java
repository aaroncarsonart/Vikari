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
import static com.atonementcrystals.dnr.vikari.TestUtils.assertNoWarnings;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TreeWalkInterpreterTest_LineContinuations extends TreeWalkInterpreterPrintTest_Base {

    /**
     * Helper method to efficiently test Vikari print statements.
     *
     * @param sourceString   The Vikari source code to execute.
     * @param expectedOutput The expected output for the print statements.
     */
    public void testPrintStatement(String sourceString, String expectedOutput) {
        for (int i = 0; i < 2; i++) {
            boolean warningsEnabled = i == 0;

            Lexer lexer = new Lexer();
            Parser parser = new Parser();
            TreeWalkInterpreter interpreter = new TreeWalkInterpreter();

            lexer.setCompilationWarningsEnabled(warningsEnabled);

            SyntaxErrorReporter syntaxErrorReporter = new SyntaxErrorReporter();
            lexer.setSyntaxErrorReporter(syntaxErrorReporter);
            parser.setSyntaxErrorReporter(syntaxErrorReporter);
            interpreter.setGetLineFunction(syntaxErrorReporter::getLineFromCache);

            parser.setGlobalAtonementField(globalAtonementField);
            interpreter.setGlobalAtonementField(globalAtonementField);

            List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
            List<Statement> parsedStatements = parser.parse(null, lexedStatements);

            assertNoSyntaxErrors(syntaxErrorReporter);
            assertNoWarnings(syntaxErrorReporter);
            interpreter.interpret(null, parsedStatements);

            testOutput(expectedOutput);
        }
    }

    @Test
    @Order(1)
    public void testTreeWalkInterpreter_LineContinuation_VariableDeclaration() {
        String expectedOutput = "5\n";

        // One line continuation
        testPrintStatement("foo~\n:Integer << 2 + 3\n:foo:", expectedOutput);
        testPrintStatement("foo:~\nInteger << 2 + 3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer~\n<< 2 + 3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer <<~\n2 + 3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer << 2~\n+ 3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer << 2 +~\n3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer << 2 + 3\n:~\nfoo:", expectedOutput);
        testPrintStatement("foo:Integer << 2 + 3\n:foo~\n:", expectedOutput);

        // Two line continuations
        testPrintStatement("foo~\n:Integer~\n<< 2 + 3\n:foo:", expectedOutput);
        testPrintStatement("foo:~\nInteger <<~\n2 + 3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer~\n<< 2~\n+ 3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer <<~\n2 +~\n3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer << 2~\n+ 3\n:~\nfoo:", expectedOutput);
        testPrintStatement("foo:Integer << 2 +~\n3\n:foo~\n:", expectedOutput);

        // Three line continuations
        testPrintStatement("foo~\n:Integer~\n<< 2~\n+ 3\n:foo:", expectedOutput);
        testPrintStatement("foo:~\nInteger <<~\n2 +~\n3\n:foo:", expectedOutput);
        testPrintStatement("foo:Integer~\n<< 2~\n+ 3\n:~\nfoo:", expectedOutput);
        testPrintStatement("foo:Integer <<~\n2 +~\n3\n:foo~\n:", expectedOutput);

        // All line continuations
        testPrintStatement("foo~\n:~\nInteger~\n<<~\n2~\n+~\n3\n:~\nfoo~\n:", expectedOutput);
    }

    @Test
    @Order(2)
    public void testTreeWalkInterpreter_LineContinuation_AssignmentStatement_WithStatementSeparators() {
        String expectedOutput = "6.28\n";

        // One line continuation
        testPrintStatement("foo~\n:Float, foo << [3.14 * 2], :foo:", expectedOutput);
        testPrintStatement("foo:~\nFloat, foo << [3.14 * 2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo~\n<< [3.14 * 2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo <<~\n[3.14 * 2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo <<[~\n3.14 * 2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [3.14~\n* 2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [3.14 *~\n2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [3.14 * 2~\n], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [3.14 * 2], :~\nfoo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [3.14 * 2], :foo~\n:", expectedOutput);

        // Two line continuations
        testPrintStatement("foo~\n:Float, foo <<~\n[3.14 * 2], :foo:", expectedOutput);
        testPrintStatement("foo:~\nFloat, foo << [~\n3.14 * 2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo~\n<< [3.14~\n* 2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo <<~\n[3.14 *~\n2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [~\n3.14 * 2~\n], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [3.14~\n* 2], :~\nfoo:", expectedOutput);
        testPrintStatement("foo:Float, foo << [3.14 *~\n2], :foo~\n:", expectedOutput);

        // Three line continuations
        testPrintStatement("foo~\n:Float, foo~\n<< [3.14~\n* 2], :foo:", expectedOutput);
        testPrintStatement("foo:~\nFloat, foo <<~\n[3.14 *~\n2], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo~\n<< [~\n3.14 * 2~\n], :foo:", expectedOutput);
        testPrintStatement("foo:Float, foo <<~\n[3.14~\n* 2], :~\nfoo:", expectedOutput);
        testPrintStatement("foo:Float, foo~\n<< [3.14 *~\n2], :foo~\n:", expectedOutput);

        // All line continuations
        testPrintStatement("foo~\n:~\nFloat, foo~\n<<~\n[~\n3.14~\n*~\n2~\n], :~\nfoo~\n:", expectedOutput);
    }

    @Test
    @Order(3)
    public void testTreeWalkInterpreter_LineContinuation_PrintStatement_WithComment() {
        String expectedOutput = "57\n";

        // One line continuation
        testPrintStatement(":~\n5~:Comment.:~:7:", expectedOutput);
        testPrintStatement(":5~:Comment.:~~\n:7:", expectedOutput);
        testPrintStatement(":5~:Comment.:~:~\n7:", expectedOutput);
        testPrintStatement(":5~:Comment.:~:7~\n:", expectedOutput);

        // Two line continuations
        testPrintStatement(":~\n5~:Comment.:~~\n:7:", expectedOutput);
        testPrintStatement(":5~\n~:Comment.:~:~\n7:", expectedOutput);
        testPrintStatement(":5~:Comment.:~~\n:7~\n:", expectedOutput);

        // Three line continuations
        testPrintStatement(":~\n5~:Comment.:~~\n:7~\n:", expectedOutput);

        // All line continuations
        testPrintStatement(":~\n5~\n~:Comment.:~~\n:~\n7~\n:", expectedOutput);
    }

    @Test
    @Order(4)
    public void testTreeWalkInterpreter_LineContinuation_PrintStatement_WithMultiLineComment() {
        String expectedOutput = "57\n";

        // One line continuation
        testPrintStatement(":~\n5~:Multi-line\ncomment.:~:7:", expectedOutput);
        testPrintStatement(":5~:Multi-line\ncomment.:~~\n:7:", expectedOutput);
        testPrintStatement(":5~:Multi-line\ncomment.:~:~\n7:", expectedOutput);
        testPrintStatement(":5~:Multi-line\ncomment.:~:7~\n:", expectedOutput);

        // Two line continuations
        testPrintStatement(":~\n5~:Multi-line\ncomment.:~~\n:7:", expectedOutput);
        testPrintStatement(":5~\n~:Multi-line\ncomment.:~:~\n7:", expectedOutput);
        testPrintStatement(":5~:Multi-line\ncomment.:~~\n:7~\n:", expectedOutput);

        // Three line continuations
        testPrintStatement(":~\n5~:Multi-line\ncomment.:~~\n:7~\n:", expectedOutput);

        // All line continuations
        testPrintStatement(":~\n5~\n~:Multi-line\ncomment.:~~\n:~\n7~\n:", expectedOutput);
    }
}
