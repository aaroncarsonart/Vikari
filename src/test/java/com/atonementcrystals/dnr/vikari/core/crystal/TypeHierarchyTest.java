package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class TypeHierarchyTest {

    private void testInheritance(VikariType candidateChildType, VikariType candidateParent) {
        TypeCrystal candidateChildTypeCrystal = candidateChildType.getTypeCrystal();
        TypeCrystal candidateParentTypeCrystal = candidateParent.getTypeCrystal();

        boolean inheritanceResult = TypeCrystal.checkInheritance(candidateChildTypeCrystal, candidateParentTypeCrystal);
        assertTrue(inheritanceResult, "Unexpected inheritance result.");
    }

    private void testChildren(VikariType parentType, VikariType... childrenTypes) {
        TypeCrystal parentTypeCrystal = parentType.getTypeCrystal();
        String parentTypeName = parentTypeCrystal.getTypeName();
        List<TypeCrystal> children = parentTypeCrystal.getChildren();

        int expectedChildrenCount = childrenTypes.length;
        int actualChildrenCount = children.size();
        assertEquals(expectedChildrenCount, actualChildrenCount, parentTypeName + " has unexpected number of " +
                "inheriting child types.");

        for (VikariType childType : childrenTypes) {
            TypeCrystal childTypeCrystal = childType.getTypeCrystal();
            String childTypeName = childTypeCrystal.getTypeName();
            assertTrue(children.contains(childTypeCrystal), "Expected " + childTypeName + " to be an inherited child " +
                    "of " + parentTypeName + ".");
        }
    }

    /**
     * Test that all TypeCrystals created by VikariType have a properly-instantiated type hierarchy.
     */
    @Test
    @Order(1)
    public void testInit() {
        TypeHierarchy.init();

        testInheritance(VikariType.TYPE, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.VALUE, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.BOOLEAN, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.NUMBER, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.INTEGER, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.LONG, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.BIG_INTEGER, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.FLOAT, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.DOUBLE, VikariType.ATONEMENT_CRYSTAL);
        testInheritance(VikariType.BIG_DECIMAL, VikariType.ATONEMENT_CRYSTAL);

        testInheritance(VikariType.BOOLEAN, VikariType.VALUE);
        testInheritance(VikariType.NUMBER, VikariType.VALUE);
        testInheritance(VikariType.INTEGER, VikariType.VALUE);
        testInheritance(VikariType.LONG, VikariType.VALUE);
        testInheritance(VikariType.BIG_INTEGER, VikariType.VALUE);
        testInheritance(VikariType.FLOAT, VikariType.VALUE);
        testInheritance(VikariType.DOUBLE, VikariType.VALUE);
        testInheritance(VikariType.BIG_DECIMAL, VikariType.VALUE);

        testInheritance(VikariType.INTEGER, VikariType.NUMBER);
        testInheritance(VikariType.LONG, VikariType.NUMBER);
        testInheritance(VikariType.BIG_INTEGER, VikariType.NUMBER);
        testInheritance(VikariType.FLOAT, VikariType.NUMBER);
        testInheritance(VikariType.DOUBLE, VikariType.NUMBER);
        testInheritance(VikariType.BIG_DECIMAL, VikariType.NUMBER);
    }

    @Test
    @Order(2)
    public void testChildren_AfterMultipleInitCalls() {
        TypeHierarchy.init();
        TypeHierarchy.init();

        testChildren(VikariType.ATONEMENT_CRYSTAL,
                VikariType.TYPE,
                VikariType.VALUE);

        testChildren(VikariType.TYPE);

        testChildren(VikariType.VALUE,
                VikariType.BOOLEAN,
                VikariType.NUMBER);

        testChildren(VikariType.NUMBER,
                VikariType.INTEGER,
                VikariType.LONG,
                VikariType.BIG_INTEGER,
                VikariType.FLOAT,
                VikariType.DOUBLE,
                VikariType.BIG_DECIMAL);
    }
}
