package com.atonement.crystals.dnr.vikari.core.comment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

/**
 * A comment crystal contains the contents of a comment escaped by
 * `~:` and `:~`.
 */
public class CommentCrystal extends AtonementCrystal {

    public CommentCrystal(String contents) {
        super(contents);
    }
}
