package com.atonementcrystals.dnr.vikari.core.crystal.operator.prefix;


import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The cast operator ! accepts a variable reference as its left operand
 * and a Type reference as its right operand. It then casts the variable
 * to the new Type. This is only allowed if the variable can be safely
 * converted to the new type.<br/>
 * <pre>
 * float:Float << 123.456
 * :float!Int: ~:prints 123:~</pre>
 *
 * A type may be safely cast to any type within its inheritance hierarchy.
 * Value types can also all be safely converted between one another.
 * Any type may also be cast to any other type by defining a function with
 * the <code>$:CastTo{Type}</code> annotation.<br/>
 * <pre>
 * ~:The first form can defined outside of a crystal's type declaration.:~
 * $:CastTo{Integer}
 * castToInteger << (crystal:Crystal): Integer ::
 *   ^^ crystal.getId!()
 *
 * ~:The second form must be defined within a crystal's type declaration.:~
 * $:CastTo{Integer}
 * getId << (): Integer ::
 *   ^^ @id</pre>
 *
 * All types can be safely cast to a String according to that type's
 * string!() function. Casting to a Boolean by default returns true for
 * non-null values, and false for nulls. Casting to a concrete numeric
 * type returns 1 for non-null or true values, and 0 for null or false
 * values. String representations of Value types which are equivalent to
 * their literal values in Vikari, such as 1028, 3.14159265, true, null,
 * etc. when cast to that Value type hold the literal value given by
 * that string representation. If the String is unparsable as the Value
 * type being cast to, then the default value for that type is given.<br/>
 * <br/>
 * Characters cast to Numbers using their unicode value. To get the
 * digit value of a Character as a Number, either cast first to a String
 * and then to a Number, or use the digitAsNumber!() class of functions.<br/>
 * <br/>
 * Casting a variable to a collection type will convert its field members
 * into members of the given collection according to its iteration order
 * given index of operator $. Casting to a List, Array, or Set gives you
 * a collection of the field member values. Casting to a Map provides a Set
 * of key/value Pairs, where the keys are the field member names as Strings
 * and the values are the field member values. Field members declared in
 * regions are prefixed by their region names as though the field member
 * was being referenced off the variable.<br/>
 * <br/>
 * A type reference variable may be applied as the right operand of the
 * cast operator instead of a Type name.<br/>
 * <pre>
 * foo:Integer << 1024
 * type:Type << Double
 * :foo!type: ~:prints 1024.0:~</pre>
 */
public class CastOperatorCrystal extends AtonementCrystal {

    public CastOperatorCrystal() {
        super(TokenType.CAST.getIdentifier());
    }

}