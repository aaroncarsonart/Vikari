package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.util.CoordinatePair;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AtonementCrystalTest {

    @Test
    @Order(1)
    public void testCopy() {
        AtonementCrystal crystal1 = new AtonementCrystal("foo");
        crystal1.setType(VikariType.INTEGER);
        crystal1.setCoordinates(new CoordinatePair(0, 0));
        AtonementCrystal crystal2 = crystal1.copy();

        assertEquals(crystal1.getIdentifier(), crystal2.getIdentifier(), "Expected identifiers to be equivalent.");
        assertEquals(crystal1.getField(), crystal2.getField(), "Expected fields to be equivalent.");
        assertEquals(crystal1.getInstantiatedType(), crystal2.getInstantiatedType(), "Expected instantiated types to be equivalent.");

        assertNotEquals(crystal1.getDeclaredType(), crystal2.getDeclaredType(), "Expected declared types to not be equivalent.");
        assertNotEquals(crystal1.getCoordinates(), crystal2.getCoordinates(), "Expected coordinates to not be equivalent.");
    }
}
