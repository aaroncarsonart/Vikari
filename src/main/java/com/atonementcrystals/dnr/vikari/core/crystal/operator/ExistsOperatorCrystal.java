package com.atonementcrystals.dnr.vikari.core.crystal.operator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The exists ? operator crystal tests that a crystal contains a given field member.
 * It is followed by a dereference of a potential field member name using . or ::.
 * The type label assertion is optional.<br/>
 * <pre>
 * ~:Untyped exists checks:~
 * crystal?.instanceFieldMember
 * crystal?::staticFieldMember
 *
 * ~:With a type label:~
 * crystal?.foo:Integer
 * crystal?::bar:Double
 *
 * ~:Dereferences can be safely chained together.:~
 * crystal?.a.b.c
 * crystal?::d::e::f
 * crystal?::g.h::i::j.k.l</pre>
 *
 * When an exists operator expression is used in a print statement, always enclose
 * it in a grouping to avoid ambiguity of parsing because of the optional type label.
 * The longest valid expression is always parsed, otherwise.<br/>
 * <pre>
 * :foo?.bar:Baz:    ~:Ambiguous: parsed as 1 print expression. Issues a warning.:~
 * :[foo?.bar]:Baz:  ~:Unambiguous: parsed as 2 print expressions.:~
 * :[foo?.bar:Baz]:  ~:Unambiguous: parsed as 1 print expression.:~</pre>
 *
 * The following therefore will also issue a syntax error if baz is not a Type.<br/>
 * <pre>
 * :foo?.bar:baz:    ~:Ambiguous, parsed as 1 print expression, and a potential syntax error.:~
 * :[foo?.bar]:baz:  ~:Unambiguous, no potential type errors.
 * :[foo?.bar:baz]:  ~:Unambiguous, but baz still must also be a Type reference.:~</pre>
 */
public class ExistsOperatorCrystal extends AtonementCrystal {

    public ExistsOperatorCrystal() {
        super(TokenType.EXISTS.getIdentifier());
    }

}
