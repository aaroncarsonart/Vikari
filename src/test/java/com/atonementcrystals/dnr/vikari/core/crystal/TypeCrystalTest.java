package com.atonementcrystals.dnr.vikari.core.crystal;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class TypeCrystalTest {
    private static final String PACKAGE = "test::pkg";

    @Test
    @Order(1)
    public void testInheritance_CustomTypes() {
        TypeCrystal type1 = new TypeCrystal(PACKAGE, "Type1");
        TypeCrystal type2 = new TypeCrystal(PACKAGE, "Type2");

        type1.addChildren(type2);

        // test hasType()
        assertFalse(type1.hasType(type2), "Unexpected inheritance result.");
        assertTrue(type2.hasType(type1), "Unexpected inheritance result.");

        assertTrue(type1.hasType(type1), "Expected hasType() to return true for a self-reference.");
        assertTrue(type2.hasType(type2), "Expected hasType() to return true for a self-reference.");

        // test checkInheritance()
        assertFalse(TypeCrystal.checkInheritance(type1, type2), "Unexpected inheritance result.");
        assertTrue(TypeCrystal.checkInheritance(type2, type1), "Unexpected inheritance result.");

        assertFalse(TypeCrystal.checkInheritance(type1, type1), "A type cannot inherit from itself.");
        assertFalse(TypeCrystal.checkInheritance(type2, type2), "A type cannot inherit from itself.");
    }

    @Test
    @Order(2)
    public void testInheritance_CustomTypes_MoreLevels() {
        TypeCrystal type1 = new TypeCrystal(PACKAGE, "Type1");
        TypeCrystal type2 = new TypeCrystal(PACKAGE, "Type2");
        TypeCrystal type3 = new TypeCrystal(PACKAGE, "Type3");
        TypeCrystal type4 = new TypeCrystal(PACKAGE, "Type4");

        type1.addChildren(type2);
        type2.addChildren(type3);
        type2.addChildren(type4);

        // test hasType()
        assertTrue(type2.hasType(type1), "Unexpected inheritance result.");
        assertTrue(type3.hasType(type1), "Unexpected inheritance result.");
        assertTrue(type4.hasType(type1), "Unexpected inheritance result.");
        assertTrue(type3.hasType(type2), "Unexpected inheritance result.");
        assertTrue(type4.hasType(type2), "Unexpected inheritance result.");

        assertFalse(type1.hasType(type2), "Unexpected inheritance result.");
        assertFalse(type1.hasType(type3), "Unexpected inheritance result.");
        assertFalse(type1.hasType(type4), "Unexpected inheritance result.");
        assertFalse(type2.hasType(type3), "Unexpected inheritance result.");
        assertFalse(type2.hasType(type4), "Unexpected inheritance result.");

        // test checkInheritance()
        assertFalse(TypeCrystal.checkInheritance(type1, type1), "A type cannot inherit from itself.");
        assertFalse(TypeCrystal.checkInheritance(type2, type2), "A type cannot inherit from itself.");
        assertFalse(TypeCrystal.checkInheritance(type3, type3), "A type cannot inherit from itself.");
        assertFalse(TypeCrystal.checkInheritance(type4, type4), "A type cannot inherit from itself.");

        assertTrue(TypeCrystal.checkInheritance(type2, type1), "Unexpected inheritance result.");
        assertTrue(TypeCrystal.checkInheritance(type3, type1), "Unexpected inheritance result.");
        assertTrue(TypeCrystal.checkInheritance(type4, type1), "Unexpected inheritance result.");
        assertTrue(TypeCrystal.checkInheritance(type3, type2), "Unexpected inheritance result.");
        assertTrue(TypeCrystal.checkInheritance(type4, type2), "Unexpected inheritance result.");

        assertFalse(TypeCrystal.checkInheritance(type1, type2), "Unexpected inheritance result.");
        assertFalse(TypeCrystal.checkInheritance(type1, type3), "Unexpected inheritance result.");
        assertFalse(TypeCrystal.checkInheritance(type1, type4), "Unexpected inheritance result.");
        assertFalse(TypeCrystal.checkInheritance(type2, type3), "Unexpected inheritance result.");
        assertFalse(TypeCrystal.checkInheritance(type3, type4), "Unexpected inheritance result.");
    }

    @Test
    @Order(3)
    public void testEquals() {
        // Different instances with the same fields should return false.
        // In practice only one instance of each unique type should exist.
        // But even so, duplicates should never be equal.

        TypeCrystal type1_NamedFoo = new TypeCrystal(PACKAGE, "Foo");
        TypeCrystal type2_NamedFoo = new TypeCrystal(PACKAGE, "Foo");
        TypeCrystal type3_NamedBar = new TypeCrystal(PACKAGE, "Bar");

        assertEquals(type1_NamedFoo, type1_NamedFoo, "A TypeCrystal should be equal to itself.");
        assertNotEquals(type1_NamedFoo, type2_NamedFoo, "Different TypeCrystal instances should not be equal.");
        assertNotEquals(type1_NamedFoo, type3_NamedBar, "Different TypeCrystal instances should not be equal.");
    }
}
