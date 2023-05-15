package com.atonementcrystals.dnr.vikari.error;

public class Vikari_RuntimeException extends Vikari_Exception {
    private final RuntimeError runtimeError;

    public Vikari_RuntimeException(String name, RuntimeError runtimeError) {
        super(name, runtimeError.getMessage());
        this.runtimeError = runtimeError;
    }

    public RuntimeError getRuntimeError() {
        return runtimeError;
    }
}
