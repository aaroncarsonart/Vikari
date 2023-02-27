package com.atonement.crystals.dnr.vikari.error;

public class Vikari_Error extends RuntimeException {

    private String errorName;
    private String errorMessage;

    public Vikari_Error(String name, String message) {
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
