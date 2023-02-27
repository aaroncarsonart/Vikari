package com.atonement.crystals.dnr.vikari.error;

public class Vikari_SyntaxErrorException extends Vikari_Error {

    public Vikari_SyntaxErrorException(String message) {
        super("Syntax Error", message);
    }
}
