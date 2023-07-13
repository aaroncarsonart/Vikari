package com.atonementcrystals.dnr.vikari.interpreter.jline;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.jline.reader.EOFError;
import org.jline.reader.ParsedLine;
import org.jline.reader.Parser;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class VikariJLineParserTest {

    private void ensureSingleStatement(String sourceString) {
        Lexer lexer = new Lexer();
        List<List<AtonementCrystal>> statements = lexer.lex(sourceString);
        assertTrue(statements.size() <= 1, "Malformed test. Input should only lex to zero or one statements.");
    }

    private void testThrowsEOFError(String sourceString, CoordinatePair expectedLocation, String expectedMessage) {
        ensureSingleStatement(sourceString);
        VikariJLineParser jLineParser = new VikariJLineParser();
        try {
            jLineParser.parse(sourceString, 0, Parser.ParseContext.ACCEPT_LINE);
            fail("Expected EOFError for test case.");
        } catch (EOFError e) {
            assertEquals(expectedLocation.getRow(), e.line(), "Unexpected line.");
            assertEquals(expectedLocation.getColumn(), e.column(), "Unexpected column.");
            assertEquals(expectedMessage, e.getMessage(), "Unexpected message.");
        }
    }

    private void testEndOfLine(String sourceString) {
        ensureSingleStatement(sourceString);
        VikariJLineParser jLineParser = new VikariJLineParser();
        try {
            ParsedLine parsedLine = jLineParser.parse(sourceString, 0, Parser.ParseContext.ACCEPT_LINE);
            assertNotNull(parsedLine, "Expected parsed line is not null.");
            assertEquals(VikariJLineParsedLine.class, parsedLine.getClass(), "Unexpected type for parsed line.");
            assertEquals(sourceString, parsedLine.line(), "Unexpected line for parsed line.");
        } catch (EOFError e) {
            fail("Expected no EOFError for test case.");
        }
    }

    @Test
    @Order(1)
    public void testJLineParser_OrdinaryStatements() {
        testEndOfLine("5");
        testEndOfLine("5 +");
        testEndOfLine("5 + 2");
        testEndOfLine("a");
        testEndOfLine("a:");
        testEndOfLine("a:Integer");
        testEndOfLine("a:Integer <<");
        testEndOfLine("a:Integer << 2");
        testEndOfLine(":");
        testEndOfLine(":foo");
        testEndOfLine(":foo:");
        testEndOfLine(":foo:``bar``");
        testEndOfLine(":foo:``bar``:");
        testEndOfLine("[");
        testEndOfLine("[3.14");
        testEndOfLine("[3.14 *");
        testEndOfLine("[3.14 * 2");
        testEndOfLine("[3.14 * 2]");
     }

    @Test
    @Order(2)
    public void testJLineParser_LineContinuation_OnlyLineContinuations() {
        testThrowsEOFError("~", location(0, 0), "Line continuation.");
        testThrowsEOFError("~\n~", location(1, 0), "Line continuation.");
        testThrowsEOFError("~\n~\n~", location(2, 0), "Line continuation.");
        testThrowsEOFError("~\n~\n~\n~", location(3, 0), "Line continuation.");
        testThrowsEOFError("~\n~\n~\n~\n~", location(4, 0), "Line continuation.");
    }

    @Test
    @Order(3)
    public void testJLineParser_LineContinuation_ArithmeticStatements() {
        testThrowsEOFError("a~", location(0, 1), "Line continuation.");
        testThrowsEOFError("a +~", location(0, 3), "Line continuation.");
        testThrowsEOFError("a + 5~", location(0, 5), "Line continuation.");

        testThrowsEOFError("a~\n+~", location(1, 1), "Line continuation.");
        testThrowsEOFError("a~\n+~\n5~", location(2, 1), "Line continuation.");

        testThrowsEOFError("~\na~", location(1, 1), "Line continuation.");
        testThrowsEOFError("~\na +~", location(1, 3), "Line continuation.");
        testThrowsEOFError("~\na + 5~", location(1, 5), "Line continuation.");

        testThrowsEOFError("~\na~\n+~", location(2, 1), "Line continuation.");
        testThrowsEOFError("~\na~\n+~\n5~", location(3, 1), "Line continuation.");
    }

    @Test
    @Order(4)
    public void testJLineParser_LineContinuation_ArithmeticStatement_TerminatedByNewline() {
        testEndOfLine("~\n");

        testEndOfLine("a~\n");
        testEndOfLine("a +~\n");
        testEndOfLine("a + 5~\n");

        testEndOfLine("a~\n+~\n");
        testEndOfLine("a~\n+~\n5~\n");

        testEndOfLine("~\na~\n");
        testEndOfLine("~\na +~\n");
        testEndOfLine("~\na + 5~\n");

        testEndOfLine("~\na~\n+~\n");
        testEndOfLine("~\na~\n+~\n5~\n");
    }

    @Test
    @Order(5)
    public void testJLineParser_UnclosedComments() {
        testThrowsEOFError("~:", location(0, 0), "Unclosed comment.");
        testThrowsEOFError("~:comment", location(0, 0), "Unclosed comment.");
        testThrowsEOFError("~:comment 1 ~:comment 2:~", location(0, 0), "Unclosed comment.");

        testThrowsEOFError("a ~:", location(0, 2), "Unclosed comment.");
        testThrowsEOFError("a + ~:", location(0, 4), "Unclosed comment.");
        testThrowsEOFError("a + 5 ~:", location(0, 6), "Unclosed comment.");

        testThrowsEOFError("a: ~:", location(0, 3), "Unclosed comment.");
        testThrowsEOFError("a:Integer ~:", location(0, 10), "Unclosed comment.");
        testThrowsEOFError("a:Integer << ~:", location(0, 13), "Unclosed comment.");
        testThrowsEOFError("a:Integer << 2 ~:", location(0, 15), "Unclosed comment.");
        testThrowsEOFError("a:Integer << 2 + ~:", location(0, 17), "Unclosed comment.");
        testThrowsEOFError("a:Integer << 2 + 5 ~:", location(0, 19), "Unclosed comment.");

        testThrowsEOFError("a: ~:\ncomment:~ Integer ~:", location(1, 18), "Unclosed comment.");
        testThrowsEOFError("~:\nmulti-\nline\ncomment:~ a:Integer ~:", location(3, 20), "Unclosed comment.");
        testThrowsEOFError("~:comment1~:comment2:~:~~:comment3~:\ncomment4\n:~", location(0, 24), "Unclosed comment.");
    }

    @Test
    @Order(6)
    public void testJLineParser_ClosedComments() {
        testEndOfLine("~:comment:~");
        testEndOfLine("~:comment 1 ~:comment 2:~ :~");
        testEndOfLine("~:comment 1 ~:comment 2:~ :~");
        testEndOfLine("~:multi-line\ncomment:~");
        testEndOfLine("~:multi-line comment 1 ~:\nmulti-line comment 2:~\n:~");
        testEndOfLine("~:multi-line comment \n~:comment:~ :~");
        testEndOfLine("~:multi-line comment ~:comment:~\n:~");

        testEndOfLine("a: ~:comment:~ Integer");
        testEndOfLine("a: ~:comment 1 ~:comment 2:~ :~ Integer");
        testEndOfLine("a: ~:comment 1 ~:comment 2:~ :~ Integer");
        testEndOfLine("a: ~:multi-line\ncomment:~ Integer");
        testEndOfLine("a: ~:multi-line comment 1 ~:\nmulti-line comment 2:~\n:~ Integer");
        testEndOfLine("a: ~:multi-line comment \n~:comment:~ :~ Integer");
        testEndOfLine("a: ~:multi-line comment ~:comment:~\n:~ Integer");
    }

    @Test
    @Order(7)
    public void testJLineParser_LineContinuation_WithCommentsAfter() {
        testThrowsEOFError("~\n~:", location(1, 0), "Unclosed comment.");
        testThrowsEOFError("a~\n~:", location(1, 0), "Unclosed comment.");
        testThrowsEOFError("a +~\n~:", location(1, 0), "Unclosed comment.");
        testThrowsEOFError("a + 5~\n~:", location(1, 0), "Unclosed comment.");

        testThrowsEOFError("a~\n~:~:", location(1, 0), "Unclosed comment.");
        testThrowsEOFError("a~\n~:comment 1 ~:comment 2", location(1, 0), "Unclosed comment.");
        testThrowsEOFError("a~\n~:comment 1 ~:comment 2:~", location(1, 0), "Unclosed comment.");

        testEndOfLine("a~\n~:comment:~");
        testEndOfLine("a +~\n~:comment:~");
        testEndOfLine("a + 5~\n~:comment:~");

        testEndOfLine("a~\n~:comment 1 ~:comment 2:~ :~");
        testEndOfLine("a +~\n~:comment ~:comment 2:~ :~");
        testEndOfLine("a + 5~\n~:comment ~:comment 2:~ :~");
    }
}
