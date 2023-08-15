package com.atonementcrystals.dnr.vikari.util;

public class TokenPosition {
    private final int lineNumber;
    private final int tokenNumber;

    public TokenPosition(int lineNumber, int tokenNumber) {
        this.lineNumber = lineNumber;
        this.tokenNumber = tokenNumber;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public int getTokenNumber() {
        return tokenNumber;
    }
}
