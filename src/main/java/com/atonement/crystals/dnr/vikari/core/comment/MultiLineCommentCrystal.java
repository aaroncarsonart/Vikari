package com.atonement.crystals.dnr.vikari.core.comment;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;

/**
 * A multi-line string literal is defined across more than one line.
 */
public class MultiLineCommentCrystal extends AtonementCrystal {

    private String comment;
    private MultiLineCommentCrystal next;

    public MultiLineCommentCrystal(String identifier) {
        super(identifier);
    }

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }

    public MultiLineCommentCrystal getNext() {
        return next;
    }

    public void setNext(MultiLineCommentCrystal next) {
        this.next = next;
    }
}
