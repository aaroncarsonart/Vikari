package com.atonement.crystals.dnr.vikari.core.crystal.separator;

import com.atonement.crystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * A semicolon ; is a region separator. This denotes an explicit termination
 * of the innermost field region or code block.<br/>
 * <br/>
 * For indented code, these are not necessary to terminate ordinary regions
 * of code. But they can be used optionally to explicitly terminate every
 * code region.<br/>
 * <br/>
 * However, they are necessary for minimized Vikari code. Wherein
 * they are used in conjunction with the statement separator , to
 * explicitly terminate very minimized region of Vikari code.
 */
public class RegionSeparatorCrystal extends AtonementCrystal {

    public RegionSeparatorCrystal() {
        super(TokenType.REGION_SEPARATOR.getIdentifier());
    }

}
