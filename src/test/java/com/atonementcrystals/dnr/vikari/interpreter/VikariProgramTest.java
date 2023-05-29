package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementField;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.VikariType;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static org.junit.jupiter.api.Assertions.assertTrue;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class VikariProgramTest {

    @Test
    @Order(1)
    public void testGlobalAtonementField() {
        VikariProgram vikariProgram = new VikariProgram();
        AtonementField globalAtonementField = vikariProgram.getGlobalAtonementField();

        for (VikariType vikariType : VikariType.LANG_TYPES) {
            String typeName = vikariType.getTypeCrystal().getTypeName();
            assertTrue(globalAtonementField.isDefined(typeName), "Expected a lang type to be defined for the global " +
                    "Atonement Field using its shortened name.");

            String fullyQualifiedTypeName = vikariType.getTypeCrystal().getFullyQualifiedTypeName();
            assertTrue(globalAtonementField.isDefined(fullyQualifiedTypeName), "Expected a lang type to be defined " +
                    "for the global Atonement Field using its fully-qualified names.");
        }
    }
}
