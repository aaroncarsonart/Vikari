package com.atonement.crystals.dnr.vikari;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

/**
 * @Author aaron
 */
public class HelloWorldTest {
    private static final String HELLO_WORLD_STATEMENT = ":``Hello, world!``";

    private final ByteArrayOutputStream outputContent = new ByteArrayOutputStream();
    private final PrintStream originalOutput = System.out;

    @BeforeEach
    public void setupOutputStreams() {
        System.setOut(new PrintStream(outputContent));
    }

    @Test
    public void testKnowledgeOperator() {
        // need to grab handles for statement parsers
        // and statement executors
    }

    @AfterEach
    public void restoreOutputStreams() {
        System.setOut(originalOutput);
    }
}
