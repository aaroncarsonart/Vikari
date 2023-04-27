package com.atonement.crystals.dnr.vikari.util;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class CoordinatePairTest {

    @Test
    @Order(1)
    public void testCompareTo() {
        CoordinatePair pair1 = new CoordinatePair(0, 0);
        CoordinatePair pair2 = new CoordinatePair(0, 0);

        // Should be equal to itself.
        assertTrue(pair1.compareTo(pair2) == 0, "Equivalent values should be equivalent.");
        assertTrue(pair2.compareTo(pair1) == 0, "Equivalent values should be equivalent.");

        // A pair with a higher column should be greater.
        pair2 = new CoordinatePair(1, 0);
        assertTrue(pair1.compareTo(pair2) < 0, "A higher column value should be greater.");
        assertTrue(pair2.compareTo(pair1) > 0, "A higher column value should be greater.");

        // A pair with a higher row should be greater.
        pair2 = new CoordinatePair(0, 1);
        assertTrue(pair1.compareTo(pair2) < 0, "A higher row value should be greater.");
        assertTrue(pair2.compareTo(pair1) > 0, "A higher row value should be greater.");

        // A pair with a higher column and row should be greater.
        pair2 = new CoordinatePair(1, 1);
        assertTrue(pair1.compareTo(pair2) < 0, "A higher column and row value should be greater.");
        assertTrue(pair2.compareTo(pair1) > 0, "A higher column and row value should be greater.");
    }
}
