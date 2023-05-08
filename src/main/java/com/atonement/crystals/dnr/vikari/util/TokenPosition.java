package com.atonement.crystals.dnr.vikari.util;

public class TokenPosition {
    private int lineNumber;
    private int tokenNumber;

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
