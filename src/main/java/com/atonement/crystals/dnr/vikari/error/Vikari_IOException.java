package com.atonement.crystals.dnr.vikari.error;

public class Vikari_IOException extends Vikari_Error {

    public Vikari_IOException(String message) {
        super("IO Error", message);
    }
}
