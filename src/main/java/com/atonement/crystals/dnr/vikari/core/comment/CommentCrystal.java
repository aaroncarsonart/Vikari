package com.atonement.crystals.dnr.vikari.core.comment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

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
