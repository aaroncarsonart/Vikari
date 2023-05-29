package com.atonementcrystals.dnr.vikari.error;

public class Vikari_FieldMemberExistsException extends Vikari_AtonementFieldException {

    public Vikari_FieldMemberExistsException() {
        super("Field member is already defined.");
    }

}

