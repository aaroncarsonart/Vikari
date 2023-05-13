package com.atonementcrystals.dnr.vikari.core.crystal.comment;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;

/**
 * A comment crystal contains the contents of a comment escaped by
 * `~:` and `:~`.
 */
public class CommentCrystal extends AtonementCrystal {

    String contents;

    public CommentCrystal(String identifier) {
        super(identifier);
    }

    public String getContents() {
        return contents;
    }

    public void setComment(String contents) {
        this.contents = contents;
    }
}
