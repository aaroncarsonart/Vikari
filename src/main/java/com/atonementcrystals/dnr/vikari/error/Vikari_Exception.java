package com.atonementcrystals.dnr.vikari.error;

public class Vikari_Exception extends RuntimeException {

    private final String errorName;
    private final String errorMessage;

    public Vikari_Exception(String name, String message) {
        super(name + ": " + message);
        this.errorName = name;
        this.errorMessage = message;
    }

    public String getErrorName() {
        return errorName;
    }

    public String getErrorMessage() {
        return errorMessage;
    }
}
