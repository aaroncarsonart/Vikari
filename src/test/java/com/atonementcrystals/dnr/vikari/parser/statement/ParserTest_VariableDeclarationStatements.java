package com.atonementcrystals.dnr.vikari.parser.statement;

import com.atonementcrystals.dnr.vikari.TestUtils;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.BinaryOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.operator.assignment.LeftAssignmentOperatorCrystal;
import com.atonementcrystals.dnr.vikari.core.expression.Expression;
import com.atonementcrystals.dnr.vikari.core.expression.LiteralExpression;
import com.atonementcrystals.dnr.vikari.core.statement.Statement;
import com.atonementcrystals.dnr.vikari.core.statement.VariableDeclarationStatement;
import com.atonementcrystals.dnr.vikari.error.SyntaxError;
import com.atonementcrystals.dnr.vikari.error.SyntaxErrorReporter;
import com.atonementcrystals.dnr.vikari.interpreter.Arithmetic;
import com.atonementcrystals.dnr.vikari.interpreter.Lexer;
import com.atonementcrystals.dnr.vikari.interpreter.Parser;
import com.atonementcrystals.dnr.vikari.interpreter.VikariProgram;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.List;

import static com.atonementcrystals.dnr.vikari.TestUtils.location;
import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ParserTest_VariableDeclarationStatements {
    private final AtonementField globalAtonementField = VikariProgram.initGlobalAtonementField();
    private AtonementField rootEnvironment;
    SyntaxErrorReporter syntaxErrorReporter;

    public List<Statement> lexAndParse(String sourceString) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        parser.setGlobalAtonementField(globalAtonementField);

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertNoSyntaxErrors(syntaxErrorReporter);
        rootEnvironment = parser.getRootEnvironment();

        return parsedStatements;
    }

    public List<Statement> lexAndParse_WithErrors(String sourceString, int expectedErrorCount) {
        Lexer lexer = new Lexer();
        Parser parser = new Parser();

        parser.setGlobalAtonementField(globalAtonementField);

        syntaxErrorReporter = new SyntaxErrorReporter();
        lexer.setSyntaxErrorReporter(syntaxErrorReporter);
        parser.setSyntaxErrorReporter(syntaxErrorReporter);

        List<List<AtonementCrystal>> lexedStatements = lexer.lex(sourceString);
        List<Statement> parsedStatements = parser.parse(null, lexedStatements);

        TestUtils.assertSyntaxErrors(syntaxErrorReporter, expectedErrorCount);
        rootEnvironment = parser.getRootEnvironment();

        return parsedStatements;
    }

    /**
     * Set localEnvironment to null for the redefined variable error case.
     */
    public void testVariableCrystal(AtonementCrystal variable, String expectedIdentifier,
                                    VikariType expectedDeclaredType, VikariType expectedInstantiatedType,
                                    CoordinatePair expectedCoordinates, AtonementField localEnvironment) {
        // Check variable from expression
        assertEquals(expectedIdentifier, variable.getIdentifier(), "Unexpected variable identifier.");
        assertEquals(expectedDeclaredType.getTypeCrystal(), variable.getDeclaredType(), "Unexpected declared type.");

        if (expectedInstantiatedType == null) {
            assertNull(variable.getInstantiatedType(), "Expected instantiated type to be null.");
        } else{
            assertEquals(expectedInstantiatedType.getTypeCrystal(), variable.getInstantiatedType(), "Unexpected instantiated type.");
        }

        assertEquals(expectedCoordinates, variable.getCoordinates(), "Unexpected coordinates.");

        // Check variable exists in local environment
        if (localEnvironment != null) {
            assertTrue(localEnvironment.isDefined(expectedIdentifier), "Expected variable to be defined in local environment.");
            assertTrue(localEnvironment.hasFieldMember(expectedIdentifier), "Expected variable to be a field member of the local environment.");

            AtonementCrystal fieldMember = localEnvironment.get(expectedIdentifier);
            assertEquals(variable, fieldMember, "Expected variable to be equal to field member of the local environment.");
        }
    }

    public void testDeclaration(Statement statement,
                                String identifier, VikariType declaredType, VikariType instantiatedType, CoordinatePair location) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, instantiatedType, location, rootEnvironment);
        assertNull(declarationStatement.getAssignmentOperator(), "Expected operator to be null.");
        assertNull(declarationStatement.getInitializerExpression(), "Expected initializer expression to be null.");
    }

    public void testDeclaration(Statement statement,
                                String identifier, VikariType declaredType, VikariType instantiatedType, CoordinatePair location,
                                Object value) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, instantiatedType, location, rootEnvironment);

        // test operator
        BinaryOperatorCrystal expectedOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, expectedOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        TestUtils.testNumberCrystal(literal, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
    }

    /**
     * Test the form of a redeclaration of a variable. Such statements should still be parsed and well-formed.
     */
    public void testDeclaration_DuplicateError(Statement statement, String identifier, VikariType declaredType,
                                               VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        testVariableCrystal(variable, identifier, declaredType, instantiatedType, location, null);

        // test operator
        BinaryOperatorCrystal expectedOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, expectedOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        TestUtils.testNumberCrystal(literal, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
    }

    /**
     * Test a declaration where the assignment does not match the type of the declared variable.
     */
    public void testDeclaration_TypeError(Statement statement, String identifier, VikariType declaredType,
                                          VikariType instantiatedType, CoordinatePair location, Object value) {

        assertEquals(VariableDeclarationStatement.class, statement.getClass(), "Unexpected statement type.");
        VariableDeclarationStatement declarationStatement = (VariableDeclarationStatement) statement;

        assertEquals(location, statement.getLocation(), "Unexpected location.");

        // test variable
        AtonementCrystal variable = declarationStatement.getDeclaredVariable();
        VikariType variableInstantiatedType = null;
        testVariableCrystal(variable, identifier, declaredType, variableInstantiatedType, location, null);

        // test operator
        BinaryOperatorCrystal expectedOperator = declarationStatement.getAssignmentOperator();
        assertEquals(LeftAssignmentOperatorCrystal.class, expectedOperator.getClass(), "Unexpected operator type.");

        // test initializer
        Expression initializerExpression = declarationStatement.getInitializerExpression();
        assertEquals(LiteralExpression.class, initializerExpression.getClass(), "Unexpected initializer expression " +
                "type.");

        AtonementCrystal literal = ((LiteralExpression) initializerExpression).getValue();
        TestUtils.testNumberCrystal(literal, value, (Class<? extends NumberCrystal>) instantiatedType.getJavaType());
    }

    @Test
    @Order(1)
    public void testParser_Statement_VariableDeclaration_Basic() {
        String sourceString = "foo";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, null, location(0, 0));
    }

    @Test
    @Order(2)
    public void testParser_Statement_VariableDeclaration_WithTypeLabel() {
        String sourceString = "foo:Integer";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.INTEGER, null, location(0, 0));
    }

    @Test
    @Order(3)
    public void testParser_Statement_VariableDeclaration_WithAssignment() {
        String sourceString = "foo << 2";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 2);
    }

    @Test
    @Order(4)
    public void testParser_Statement_VariableDeclaration_WithTypeLabel_AndAssignment() {
        String sourceString = "foo:Integer << 2";
        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2);
    }

    public void testThreeStatements(String sourceString,
                String identifier1, VikariType declaredType1, VikariType instantiatedType1, CoordinatePair location1,
                String identifier2, VikariType declaredType2, VikariType instantiatedType2, CoordinatePair location2,
                String identifier3, VikariType declaredType3, VikariType instantiatedType3, CoordinatePair location3) {

        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 3;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(parsedStatements.get(0), identifier1, declaredType1, instantiatedType1, location1);
        testDeclaration(parsedStatements.get(1), identifier2, declaredType2, instantiatedType2, location2);
        testDeclaration(parsedStatements.get(2), identifier3, declaredType3, instantiatedType3, location3);
    }

    public void testThreeStatements(String sourceString,
            String identifier1, VikariType declaredType1, VikariType instantiatedType1, CoordinatePair location1,
            Object value1,
            String identifier2, VikariType declaredType2, VikariType instantiatedType2, CoordinatePair location2,
            Object value2,
            String identifier3, VikariType declaredType3, VikariType instantiatedType3, CoordinatePair location3,
            Object value3) {

        List<Statement> parsedStatements = lexAndParse(sourceString);

        int expectedSize = 3;
        int actualSize = parsedStatements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(parsedStatements.get(0), identifier1, declaredType1, instantiatedType1, location1, value1);
        testDeclaration(parsedStatements.get(1), identifier2, declaredType2, instantiatedType2, location2, value2);
        testDeclaration(parsedStatements.get(2), identifier3, declaredType3, instantiatedType3, location3, value3);
    }

    @Test
    @Order(5)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine() {
        testThreeStatements("foo,bar,baz",
                "foo", VikariType.ATONEMENT_CRYSTAL, null, location(0, 0),
                "bar", VikariType.ATONEMENT_CRYSTAL, null, location(0, 4),
                "baz", VikariType.ATONEMENT_CRYSTAL, null, location(0, 8));
    }

    @Test
    @Order(6)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines() {
        testThreeStatements("foo\nbar\nbaz",
                "foo", VikariType.ATONEMENT_CRYSTAL, null, location(0, 0),
                "bar", VikariType.ATONEMENT_CRYSTAL, null, location(1, 0),
                "baz", VikariType.ATONEMENT_CRYSTAL, null, location(2, 0));
    }

    @Test
    @Order(7)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine_WithTypeLabels() {
        testThreeStatements("foo:Integer,bar:Integer,baz:Integer",
                "foo", VikariType.INTEGER, null, location(0, 0),
                "bar", VikariType.INTEGER, null, location(0, 12),
                "baz", VikariType.INTEGER, null, location(0, 24));
    }

    @Test
    @Order(8)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines_WithTypeLabels() {
        testThreeStatements("foo:Integer\nbar:Integer\nbaz:Integer",
                "foo", VikariType.INTEGER, null, location(0, 0),
                "bar", VikariType.INTEGER, null, location(1, 0),
                "baz", VikariType.INTEGER, null, location(2, 0));
    }

    @Test
    @Order(9)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine_WithInitializerExpressions() {
        testThreeStatements("foo << 2, bar << 3, baz << 4",
                "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 10), 3,
                "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 20), 4);
    }

    @Test
    @Order(10)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines_WithInitializerExpressions() {
        testThreeStatements("foo << 2\nbar << 3\nbaz << 4",
                "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 3,
                "baz", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(2, 0), 4);
    }

    @Test
    @Order(11)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SingleLine_WithTypeLabels_AndInitializerExpressions() {
        testThreeStatements("foo:Integer << 2, bar:Integer << 3, baz:Integer << 4",
                "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.INTEGER, VikariType.INTEGER, location(0, 18), 3,
                "baz", VikariType.INTEGER, VikariType.INTEGER, location(0, 36), 4);
    }

    @Test
    @Order(12)
    public void testParser_Statement_VariableDeclaration_MultipleDeclarations_SeparateLines_WithTypeLabels_AndInitializerExpressions() {
        testThreeStatements("foo:Integer << 2\nbar:Integer << 3\nbaz:Integer << 4",
                "foo", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 2,
                "bar", VikariType.INTEGER, VikariType.INTEGER, location(1, 0), 3,
                "baz", VikariType.INTEGER, VikariType.INTEGER, location(2, 0), 4);
    }

    @Test
    @Order(13)
    public void testParser_Statement_VariableDeclaration_AllNumericTypes() {
        String sourceString = """
                a:Integer
                b:Long
                c:BigInteger
                d:Float
                e:Double
                f:BigDecimal
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 6;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, null, location(0, 0));
        testDeclaration(statements.get(1), "b", VikariType.LONG, null, location(1, 0));
        testDeclaration(statements.get(2), "c", VikariType.BIG_INTEGER, null, location(2, 0));
        testDeclaration(statements.get(3), "d", VikariType.FLOAT, null, location(3, 0));
        testDeclaration(statements.get(4), "e", VikariType.DOUBLE, null, location(4, 0));
        testDeclaration(statements.get(5), "f", VikariType.BIG_DECIMAL, null, location(5, 0));
    }

    @Test
    @Order(14)
    public void testParser_Statement_VariableDeclaration_AllNumericTypes_WithInitializer() {
        String sourceString = """
                a:Integer << 1
                b:Long << 2L
                c:BigInteger << 3B
                d:Float << 4.0F
                e:Double << 5.0D
                f:BigDecimal << 6.0B
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 6;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        BigInteger bigInteger = new BigInteger("3");
        BigDecimal bigDecimal = new BigDecimal("6.0", Arithmetic.getMathContext());

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration(statements.get(1), "b", VikariType.LONG, VikariType.LONG, location(1, 0), 2L);
        testDeclaration(statements.get(2), "c", VikariType.BIG_INTEGER, VikariType.BIG_INTEGER, location(2, 0),
                bigInteger);
        testDeclaration(statements.get(3), "d", VikariType.FLOAT, VikariType.FLOAT, location(3, 0), 4.0F);
        testDeclaration(statements.get(4), "e", VikariType.DOUBLE, VikariType.DOUBLE, location(4, 0), 5.0D);
        testDeclaration(statements.get(5), "f", VikariType.BIG_DECIMAL, VikariType.BIG_DECIMAL, location(5, 0),
                bigDecimal);
    }

    @Test
    @Order(15)
    public void testParser_Statement_VariableDeclaration_NumericUpcasts() {
        String sourceString = """
                a:Long << 1
                b:BigInteger << 2
                c:BigInteger << 3L
                d:Float << 4
                e:Float << 5L
                f:Float << 6B
                g:Double << 7
                h:Double << 8L
                i:Double << 9B
                j:Double << 10.0F
                k:BigDecimal << 11
                l:BigDecimal << 12L
                m:BigDecimal << 13B
                n:BigDecimal << 14F
                o:BigDecimal << 15D
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 15;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.LONG, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration(statements.get(1), "b", VikariType.BIG_INTEGER, VikariType.INTEGER, location(1, 0), 2);
        testDeclaration(statements.get(2), "c", VikariType.BIG_INTEGER, VikariType.LONG, location(2, 0), 3L);
        testDeclaration(statements.get(3), "d", VikariType.FLOAT, VikariType.INTEGER, location(3, 0), 4);
        testDeclaration(statements.get(4), "e", VikariType.FLOAT, VikariType.LONG, location(4, 0), 5L);
        testDeclaration(statements.get(5), "f", VikariType.FLOAT, VikariType.BIG_INTEGER, location(5, 0), new BigInteger("6"));
        testDeclaration(statements.get(6), "g", VikariType.DOUBLE, VikariType.INTEGER, location(6, 0), 7);
        testDeclaration(statements.get(7), "h", VikariType.DOUBLE, VikariType.LONG, location(7, 0), 8L);
        testDeclaration(statements.get(8), "i", VikariType.DOUBLE, VikariType.BIG_INTEGER, location(8, 0), new BigInteger("9"));
        testDeclaration(statements.get(9), "j", VikariType.DOUBLE, VikariType.FLOAT, location(9, 0), 10.0F);
        testDeclaration(statements.get(10), "k", VikariType.BIG_DECIMAL, VikariType.INTEGER, location(10, 0), 11);
        testDeclaration(statements.get(11), "l", VikariType.BIG_DECIMAL, VikariType.LONG, location(11, 0), 12L);
        testDeclaration(statements.get(12), "m", VikariType.BIG_DECIMAL, VikariType.BIG_INTEGER, location(12, 0), new BigInteger("13"));
        testDeclaration(statements.get(13), "n", VikariType.BIG_DECIMAL, VikariType.FLOAT, location(13, 0), 14.0F);
        testDeclaration(statements.get(14), "o", VikariType.BIG_DECIMAL, VikariType.DOUBLE, location(14, 0), 15.0D);
    }

    @Test
    @Order(16)
    public void testParser_Statement_VariableDeclaration_NumericDowncasts() {
        String sourceString = """
                a:Integer << 1L
                b:Integer << 2B
                c:Integer << 3.0F
                d:Integer << 4.0D
                e:Integer << 5.0B
                f:Long << 6B
                g:Long << 7.0F
                h:Long << 8.0D
                i:Long << 9.0B
                j:BigInteger << 10.0F
                k:BigInteger << 11.0D
                l:BigInteger << 12.0B
                m:Float << 13.0D
                n:Float << 14.0B
                o:Double << 15.0B
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 15;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        MathContext mathContext = Arithmetic.getMathContext();

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, VikariType.LONG, location(0, 0), 1L);
        testDeclaration(statements.get(1), "b", VikariType.INTEGER, VikariType.BIG_INTEGER, location(1, 0), new BigInteger("2"));
        testDeclaration(statements.get(2), "c", VikariType.INTEGER, VikariType.FLOAT, location(2, 0), 3.0F);
        testDeclaration(statements.get(3), "d", VikariType.INTEGER, VikariType.DOUBLE, location(3, 0), 4.0D);
        testDeclaration(statements.get(4), "e", VikariType.INTEGER, VikariType.BIG_DECIMAL, location(4, 0), new BigDecimal("5.0", mathContext));
        testDeclaration(statements.get(5), "f", VikariType.LONG, VikariType.BIG_INTEGER, location(5, 0), new BigInteger("6"));
        testDeclaration(statements.get(6), "g", VikariType.LONG, VikariType.FLOAT, location(6, 0), 7.0F);
        testDeclaration(statements.get(7), "h", VikariType.LONG, VikariType.DOUBLE, location(7, 0), 8.0D);
        testDeclaration(statements.get(8), "i", VikariType.LONG, VikariType.BIG_DECIMAL, location(8, 0), new BigDecimal("9.0", mathContext));
        testDeclaration(statements.get(9), "j", VikariType.BIG_INTEGER, VikariType.FLOAT, location(9, 0), 10.0F);
        testDeclaration(statements.get(10), "k", VikariType.BIG_INTEGER, VikariType.DOUBLE, location(10, 0), 11.0D);
        testDeclaration(statements.get(11), "l", VikariType.BIG_INTEGER, VikariType.BIG_DECIMAL, location(11, 0), new BigDecimal("12.0", mathContext));
        testDeclaration(statements.get(12), "m", VikariType.FLOAT, VikariType.DOUBLE, location(12, 0), 13.0D);
        testDeclaration(statements.get(13), "n", VikariType.FLOAT, VikariType.BIG_DECIMAL, location(13, 0), new BigDecimal("14.0", mathContext));
        testDeclaration(statements.get(14), "o", VikariType.DOUBLE, VikariType.BIG_DECIMAL, location(14, 0), new BigDecimal("15.0", mathContext));
    }

    @Test
    @Order(17)
    public void testParser_Statement_VariableDeclaration_AssignmentToParentTypes() {
        String sourceString = """
                a << 1
                b:AtonementCrystal << 2
                c:Value << 3
                d:Number << 4
                """;

        List<Statement> statements = lexAndParse(sourceString);

        int expectedSize = 4;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration(statements.get(1), "b", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(1, 0), 2);
        testDeclaration(statements.get(2), "c", VikariType.VALUE, VikariType.INTEGER, location(2, 0), 3);
        testDeclaration(statements.get(3), "d", VikariType.NUMBER, VikariType.INTEGER, location(3, 0), 4);
    }

    @Test
    @Order(18)
    public void testParser_Statement_VariableDeclaration_ErrorCase_DuplicateDeclaration_SameType() {
        String sourceString = """
                a:Integer << 1
                a:Integer << 2
                """;

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(1, 0), "a:Integer << 2", "Variable is already defined.");

        int expectedSize = 2;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "a", VikariType.INTEGER, VikariType.INTEGER, location(0, 0), 1);
        testDeclaration_DuplicateError(statements.get(1), "a", VikariType.INTEGER, VikariType.INTEGER, location(1, 0), 2);
    }

    @Test
    @Order(19)
    public void testParser_Statement_VariableDeclaration_ErrorCase_UnknownType() {
        String sourceString = """
                foo:Foo
                bar:Bar
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar", "Unknown Type.");

        int expectedSize = 2;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, null, location(0, 0));
        testDeclaration(statements.get(1), "bar", VikariType.ATONEMENT_CRYSTAL, null, location(1, 0));
    }

    @Test
    @Order(20)
    public void testParser_Statement_VariableDeclaration_ErrorCase_UnknownType_WithAssignment() {
        String sourceString = """
                foo:Foo << 22
                bar:Bar << 7L
                """;

        int expectedErrorCount = 2;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 4), "foo:Foo << 22", "Unknown Type.");
        TestUtils.testSyntaxError(syntaxErrors.get(1), location(1, 4), "bar:Bar << 7L", "Unknown Type.");

        int expectedSize = 2;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration(statements.get(0), "foo", VikariType.ATONEMENT_CRYSTAL, VikariType.INTEGER, location(0, 0), 22);
        testDeclaration(statements.get(1), "bar", VikariType.ATONEMENT_CRYSTAL, VikariType.LONG, location(1, 0), 7L);
    }

    @Test
    @Order(21)
    public void testParser_Statement_VariableDeclaration_SyntaxError_InvalidTypeAssignment() {
        String sourceString = "foo:Type << 2";

        int expectedErrorCount = 1;
        List<Statement> statements = lexAndParse_WithErrors(sourceString, expectedErrorCount);

        List<SyntaxError> syntaxErrors = syntaxErrorReporter.getSyntaxErrors();
        TestUtils.testSyntaxError(syntaxErrors.get(0), location(0, 0), "foo:Type << 2", "Variable with " +
                "type Type cannot be assigned a value of type Integer.");

        int expectedSize = 1;
        int actualSize = statements.size();
        assertEquals(expectedSize, actualSize, "Unexpected number of statements.");

        testDeclaration_TypeError(statements.get(0), "foo", VikariType.TYPE, VikariType.INTEGER, location(0, 0), 2);
    }

    // TODO: Test initializing declared variables with previously declared variables.
}
