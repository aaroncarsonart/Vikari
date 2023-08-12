package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;

import java.util.function.BiFunction;

/**
 * Interface for easily acquiring handles to the {@link Arithmetic} methods (add, subtract, etc.).
 */
public interface ArithmeticFunction extends BiFunction<NumberCrystal<?>, NumberCrystal<?>, AtonementCrystal> {}
