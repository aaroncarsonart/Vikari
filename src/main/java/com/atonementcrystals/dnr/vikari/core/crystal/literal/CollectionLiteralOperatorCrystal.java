package com.atonementcrystals.dnr.vikari.core.crystal.literal;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The collection literal operator $: when followed by a pair of parentheses
 * (), square brackets [], or curly brackets {} enclosing a list of elements
 * defined by expressions separated by the list element separator |, a new
 * collection of type List, Array, or Set respectively is instantiated.<br/>
 * <br/>
 * Sets composed entirely of Pairs defined using the key/value operator =>
 * instantiates a Map. Which is itself also a Set of Pairs.<br/>
 * <br/>
 * Examples:<br/>
 * <pre>
 * list:List << $:(1|2|3|4)
 * array:Array << $:[``foo``|``bar``|``baz``]
 * set:Set << $:{a|b|c|d}
 * Map:Map << $:{a => 1|b => 2|c => 3}</pre>
 */
public class CollectionLiteralOperatorCrystal extends AtonementCrystal {

    public CollectionLiteralOperatorCrystal() {
        super(TokenType.COLLECTION_LITERAL.getIdentifier());
    }

}
