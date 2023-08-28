package com.atonementcrystals.dnr.vikari.core.crystal.operator.comparison;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The instance of operator ? checks if the left operand is an instance of
 * the right operand. The left operand must be a reference variable. The
 * right operand must be either a type name, a type reference, or a new
 * declared typed variable that the left operand is then cast to.<br/>
 * <pre>
 * foo?Bar      ~:Is foo an instance of Bar?:~
 * foo?bar      ~:Is foo an instance of the type referenced by bar?:~
 * foo?bar:Bar  ~:Is foo an instance of Bar? If so, cast foo to Bar as bar.:~</pre>
 *
 * The third variant involving a cast can only be used within the boolean
 * expression of a conditional statement.<br/>
 * <pre>
 * ?? [foo?bar:Bar] ::
 *   :bar.baz:</pre>
 *
 * The third variant also only works for types which share the left operand's
 * type in the type hierarchy. Custom casting using the <code>$:CastTo{Type}</code>
 * annotation must be explicitly cast with ! or via the casting function.
 */
public class InstanceOfOperatorCrystal extends AtonementCrystal {

    public InstanceOfOperatorCrystal() {
        super(TokenType.INSTANCE_OF.getIdentifier());
    }

}
