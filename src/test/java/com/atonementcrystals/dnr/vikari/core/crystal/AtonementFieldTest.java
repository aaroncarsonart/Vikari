package com.atonementcrystals.dnr.vikari.core.crystal;

import com.atonementcrystals.dnr.vikari.error.Vikari_FieldMemberExistsException;
import com.atonementcrystals.dnr.vikari.error.Vikari_UndefinedFieldMemberException;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import static org.junit.jupiter.api.Assertions.*;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AtonementFieldTest {
    private static final boolean SHADOW_VARIABLES = true;
    private static final boolean DONT_SHADOW_VARIABLES = false;

    @Test
    @Order(1)
    public void testIsDefined_AndHasFieldMember() {
        AtonementField field = new AtonementField();

        assertFalse(field.isDefined("foo"), "Expected isDefined() to return false for a field with no members.");
        assertFalse(field.isDefined("bar"), "Expected isDefined() to return false for a field with no members.");
        assertFalse(field.hasFieldMember("foo"), "Expected hasFieldMember() to return false for a field with no members.");
        assertFalse(field.hasFieldMember("bar"), "Expected hasFieldMember() to return false for a field with no members.");

        field.define("foo", new AtonementCrystal("foo"));
        field.define("bar", new AtonementCrystal("bar"));

        assertTrue(field.isDefined("foo"), "Expected isDefined() to return true after defining a crystal.");
        assertTrue(field.isDefined("bar"), "Expected isDefined() to return true after defining a crystal.");
        assertTrue(field.hasFieldMember("foo"), "Expected hasFieldMember() to return true for a field with no members.");
        assertTrue(field.hasFieldMember("bar"), "Expected hasFieldMember() to return true for a field with no members.");
    }

    @Test
    @Order(2)
    public void testGet_SingleField() {
        AtonementField field = new AtonementField();

        AtonementCrystal fooCrystal = new AtonementCrystal("foo");
        AtonementCrystal barCrystal = new AtonementCrystal("bar");

        field.define("foo", fooCrystal);
        field.define("bar", barCrystal);

        assertEquals(fooCrystal, field.get("foo"), "Expected original crystal to match result of get() method.");
        assertEquals(barCrystal, field.get("bar"), "Expected original crystal to match result of get() method.");
    }

    @Test
    @Order(3)
    public void testGet_WithParent_NoShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, DONT_SHADOW_VARIABLES);

        AtonementCrystal fooCrystal = new AtonementCrystal("foo");
        AtonementCrystal barCrystal = new AtonementCrystal("bar");

        parentField.define("foo", fooCrystal);
        childField.define("bar", barCrystal);

        assertTrue(parentField.isDefined("foo"), "Expected parent field with defined member to return true for " +
                "isDefined().");

        assertTrue(childField.isDefined("foo"), "Expected child field without shadowing to return true for " +
                "isDefined() on member defined in its parent field.");

        assertFalse(parentField.isDefined("bar"), "Expected parent field without defined member to return false for " +
                "isDefined().");

        assertTrue(childField.isDefined("bar"), "Expected child field with defined member to return true for " +
                "isDefined().");

        assertTrue(parentField.hasFieldMember("foo"), "Expected parent field to return true for hasFieldMember().");
        assertFalse(childField.hasFieldMember("foo"), "Expected child field to return false for hasFieldMember().");
        assertFalse(parentField.hasFieldMember("bar"), "Expected parent field to return false for hasFieldMember().");
        assertTrue(childField.hasFieldMember("bar"), "Expected child field to return true for isDefined().");

        assertEquals(fooCrystal, childField.get("foo"), "Expected original crystal to match result of get() method.");
        assertEquals(barCrystal, childField.get("bar"), "Expected original crystal to match result of get() method.");
    }

    @Test
    @Order(4)
    public void testGet_WithParent_WithShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, SHADOW_VARIABLES);

        AtonementCrystal fooCrystal1 = new AtonementCrystal("foo");
        AtonementCrystal fooCrystal2 = new AtonementCrystal("foo");

        parentField.define("foo", fooCrystal1);
        childField.define("foo", fooCrystal2);

        assertTrue(parentField.isDefined("foo"), "Expected parent field with defined member to return true for " +
                "isDefined().");

        assertTrue(childField.isDefined("foo"), "Expected child field with shadowing to return true for isDefined() " +
                "on member defined in its parent field.");

        assertTrue(parentField.hasFieldMember("foo"), "Expected parent field to return true for hasFieldMember().");
        assertTrue(childField.hasFieldMember("foo"), "Expected child field to return true for hasFieldMember().");

        assertEquals(fooCrystal1, parentField.get("foo"), "Expected original crystal to match result of get() method.");
        assertEquals(fooCrystal2, childField.get("foo"), "Expected original crystal to match result of get() method.");
    }

    @Test
    @Order(5)
    public void testGetFieldWithDefinition_WithoutShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, DONT_SHADOW_VARIABLES);

        AtonementCrystal fooCrystal = new AtonementCrystal("foo");
        AtonementCrystal barCrystal = new AtonementCrystal("bar");

        parentField.define("foo", fooCrystal);
        childField.define("bar", barCrystal);

        assertEquals(parentField, childField.getFieldWithDefinition("foo"), "Unexpected result of " +
                "getFieldWithDefinition().");

        assertEquals(childField, childField.getFieldWithDefinition("bar"), "Unexpected result of " +
                "getFieldWithDefinition().");
    }

    @Test
    @Order(6)
    public void testGetFieldWithDefinition_WithShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, SHADOW_VARIABLES);

        AtonementCrystal fooCrystal1 = new AtonementCrystal("foo");
        AtonementCrystal fooCrystal2 = new AtonementCrystal("foo");

        parentField.define("foo", fooCrystal1);
        childField.define("foo", fooCrystal2);

        assertEquals(childField, childField.getFieldWithDefinition("foo"), "Unexpected result of " +
                "getFieldWithDefinition().");
    }

    @Test
    @Order(7)
    public void testAssign_SingleField() {
        AtonementField field = new AtonementField();

        AtonementCrystal fooCrystal1 = new AtonementCrystal("foo");
        AtonementCrystal barCrystal1 = new AtonementCrystal("bar");

        field.define("foo", fooCrystal1);
        field.define("bar", barCrystal1);

        AtonementCrystal fooCrystal2 = new AtonementCrystal("foo");
        AtonementCrystal barCrystal2 = new AtonementCrystal("bar");

        field.assign("foo", fooCrystal2);
        field.assign("bar", barCrystal2);

        assertEquals(fooCrystal2, field.get("foo"), "Expected second crystal to match result of get() method.");
        assertEquals(barCrystal2, field.get("bar"), "Expected second crystal to match result of get() method.");
    }

    @Test
    @Order(8)
    public void testAssign_WithParent_NoShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, DONT_SHADOW_VARIABLES);

        AtonementCrystal fooCrystal1 = new AtonementCrystal("foo");
        AtonementCrystal barCrystal1 = new AtonementCrystal("bar");

        parentField.define("foo", fooCrystal1);
        childField.define("bar", barCrystal1);

        AtonementCrystal fooCrystal2 = new AtonementCrystal("foo");
        AtonementCrystal barCrystal2 = new AtonementCrystal("bar");

        childField.assign("foo", fooCrystal2);
        childField.assign("bar", barCrystal2);

        assertTrue(childField.isDefined("foo"), "Expected isDefined() to return true for field with shadowing.");
        assertTrue(childField.isDefined("bar"), "Expected isDefined() to return true for field with shadowing.");
        assertTrue(parentField.isDefined("foo"), "Expected isDefined() to return true for parent field.");
        assertFalse(parentField.isDefined("bar"), "Expected isDefined() to return false for parent field.");

        assertFalse(childField.hasFieldMember("foo"), "Expected hasFieldMember() to return false for child field.");
        assertTrue(childField.hasFieldMember("bar"), "Expected hasFieldMember() to return true for child field.");
        assertTrue(parentField.hasFieldMember("foo"), "Expected hasFieldMember() to return true for parent field.");
        assertFalse(parentField.hasFieldMember("bar"), "Expected hasFieldMember() to return false for parent field.");

        assertEquals(fooCrystal2, childField.get("foo"), "Expected second crystal to match result of get() method.");
        assertEquals(barCrystal2, childField.get("bar"), "Expected second crystal to match result of get() method.");
        assertEquals(fooCrystal2, parentField.get("foo"), "Expected second crystal to match result of get() method.");
    }

    @Test
    @Order(9)
    public void testAssign_WithParent_WithShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, SHADOW_VARIABLES);

        AtonementCrystal fooCrystal1 = new AtonementCrystal("foo");
        AtonementCrystal fooCrystal2 = new AtonementCrystal("foo");
        AtonementCrystal barCrystal1 = new AtonementCrystal("bar");

        parentField.define("foo", fooCrystal1);
        childField.define("foo", fooCrystal2);
        childField.define("bar", barCrystal1);

        AtonementCrystal fooCrystal3 = new AtonementCrystal("foo");
        AtonementCrystal barCrystal2 = new AtonementCrystal("bar");

        childField.assign("foo", fooCrystal3);
        childField.assign("bar", barCrystal2);

        assertTrue(childField.isDefined("foo"), "Expected isDefined() to return true for field with shadowing.");
        assertTrue(childField.isDefined("bar"), "Expected isDefined() to return true for field with shadowing.");
        assertTrue(parentField.isDefined("foo"), "Expected isDefined() to return true for parent field.");
        assertFalse(parentField.isDefined("bar"), "Expected isDefined() to return false for parent field.");

        assertTrue(childField.hasFieldMember("foo"), "Expected hasFieldMember() to return true for child field.");
        assertTrue(childField.hasFieldMember("bar"), "Expected hasFieldMember() to return true for child field.");
        assertTrue(parentField.hasFieldMember("foo"), "Expected hasFieldMember() to return true for parent field.");
        assertFalse(parentField.hasFieldMember("bar"), "Expected hasFieldMember() to return false for parent field.");

        assertEquals(fooCrystal3, childField.get("foo"), "Expected third crystal to match result of get() method.");
        assertEquals(barCrystal2, childField.get("bar"), "Expected second crystal to match result of get() method.");
        assertEquals(fooCrystal1, parentField.get("foo"), "Expected first crystal to match result of get() method.");
    }

    @Test
    @Order(10)
    public void testGet_ErrorCase_WithoutParent() {
        AtonementField field = new AtonementField();
        try {
            field.get("foo");
            fail ("Expected exception for undefined field member.");
        } catch (Vikari_UndefinedFieldMemberException e) {
            // pass
        }
    }

    @Test
    @Order(11)
    public void testGet_ErrorCase_WithParent() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField);
        try {
            childField.get("foo");
            fail ("Expected exception for undefined field member.");
        } catch (Vikari_UndefinedFieldMemberException e) {
            // pass
        }
    }

    @Test
    @Order(12)
    public void testDefine_ErrorCase_WithoutParent() {
        AtonementField field = new AtonementField();
        field.define("foo", new AtonementCrystal("foo"));
        try {
            field.define("foo", new AtonementCrystal("foo"));
            fail ("Expected exception for redefining a field member.");
        } catch (Vikari_FieldMemberExistsException e) {
            // pass
        }
    }

    @Test
    @Order(13)
    public void testDefine_ErrorCase_WithParent_WithoutShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, DONT_SHADOW_VARIABLES);

        parentField.define("foo", new AtonementCrystal("foo"));
        childField.define("bar", new AtonementCrystal("bar"));

        // Test redefining a field member on the parent field.
        try {
            childField.define("foo", new AtonementCrystal("foo"));
            fail ("Expected exception for redefining a field member.");
        } catch (Vikari_FieldMemberExistsException e) {
            // pass
        }

        // Test redefining a field member on the child field.
        try {
            childField.define("bar", new AtonementCrystal("bar"));
            fail ("Expected exception for redefining a field member.");
        } catch (Vikari_FieldMemberExistsException e) {
            // pass
        }
    }

    @Test
    @Order(14)
    public void testDefine_ErrorCase_WithParent_WithShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, SHADOW_VARIABLES);

        parentField.define("foo", new AtonementCrystal("foo"));
        childField.define("bar", new AtonementCrystal("bar"));

        // Test redefining a field member on the parent field.
        try {
            childField.define("foo", new AtonementCrystal("foo"));
            // pass
        } catch (Vikari_FieldMemberExistsException e) {
            fail ("Expected no exception for redefining a field member of the parent field in the child field.");
        }

        // Test redefining a field member on the child field.
        try {
            childField.define("bar", new AtonementCrystal("bar"));
            fail ("Expected exception for redefining a field member in the child field.");
        } catch (Vikari_FieldMemberExistsException e) {
            // pass
        }
    }

    @Test
    @Order(15)
    public void testAssign_ErrorCase_WithoutParent() {
        AtonementField field = new AtonementField();
        try {
            field.assign("foo", new AtonementCrystal("foo"));
            fail ("Expected exception for assigning to an undefined field member.");
        } catch (Vikari_UndefinedFieldMemberException e) {
            // pass
        }
    }

    @Test
    @Order(16)
    public void testAssign_ErrorCase_WithParent_WithoutShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, DONT_SHADOW_VARIABLES);

        // Test assigning a field member on the parent field.
        try {
            parentField.assign("foo", new AtonementCrystal("foo"));
            fail ("Expected exception for assigning to an undefined field member.");
        } catch (Vikari_UndefinedFieldMemberException e) {
            // pass
        }

        // Test assigning a field member on the child field.
        try {
            childField.assign("bar", new AtonementCrystal("bar"));
            fail ("Expected exception for assigning to an undefined field member.");
        } catch (Vikari_UndefinedFieldMemberException e) {
            // pass
        }
    }

    @Test
    @Order(17)
    public void testAssign_ErrorCase_WithParent_WithShadowing() {
        AtonementField parentField = new AtonementField();
        AtonementField childField = new AtonementField(parentField, SHADOW_VARIABLES);

        // Test assigning a field member on the parent field.
        try {
            parentField.assign("foo", new AtonementCrystal("foo"));
            fail("Expected exception for assigning to an undefined field member.");
        } catch (Vikari_UndefinedFieldMemberException e) {
            // pass
        }

        // Test assigning a field member on the child field.
        try {
            childField.assign("bar", new AtonementCrystal("bar"));
            fail("Expected exception for assigning to an undefined field member.");
        } catch (Vikari_UndefinedFieldMemberException e) {
            // pass
        }
    }
}
