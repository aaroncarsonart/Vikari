package com.atonement.crystals.dnr.vikari.core.comment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

/**
 * The comment prefix crystal `~:` denotes the beginning of a comment.
 */
public class CommentPrefixCrystal extends AtonementCrystal {

    public CommentPrefixCrystal() {
        super(DefaultIdentifierMapping.COMMENT_PREFIX_CRYSTAL.getIdentifier());
    }
}
