package com.atonement.crystals.dnr.vikari.core.comment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The comment suffix crystal `:~` denotes the beginning of a comment.
 */
public class CommentSuffixCrystal extends AtonementCrystal {

    public CommentSuffixCrystal() {
        super(DefaultIdentifierMapping.COMMENT_SUFFIX_CRYSTAL.getIdentifier());
    }
}
