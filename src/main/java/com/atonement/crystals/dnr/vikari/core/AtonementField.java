package com.atonement.crystals.dnr.vikari.core;

import java.util.LinkedHashMap;

public class AtonementField {
    // members of a crystal definition
    // data, functions, operators
    private AtonementCrystal crystal;
    private LinkedHashMap<String, AtonementCrystal> fields;
    private LinkedHashMap<String, Object> functions;
    private LinkedHashMap<String, Object> operators;
}
