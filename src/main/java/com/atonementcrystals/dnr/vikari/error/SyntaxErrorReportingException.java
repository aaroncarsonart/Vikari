package com.atonementcrystals.dnr.vikari.error;

public class SyntaxErrorReportingException extends Vikari_Exception {

    public SyntaxErrorReportingException(String message) {
        super("Syntax Error Reporting Error", message);
    }

}
