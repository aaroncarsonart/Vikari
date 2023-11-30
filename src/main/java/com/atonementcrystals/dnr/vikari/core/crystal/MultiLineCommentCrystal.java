package com.atonementcrystals.dnr.vikari.core.crystal;

/**
 * For syntax-highlighting multiline comments in Vide.
 */
public class MultiLineCommentCrystal extends AtonementCrystal implements MultilineToken {
    private boolean isOpeningToken;
    private boolean isClosingToken;

    public MultiLineCommentCrystal(String identifier) {
        super(identifier);
        isClosingToken = false;
    }

    @Override
    public boolean isOpeningToken() {
        return isOpeningToken;
    }

    public void setOpeningToken(boolean openingToken) {
        isOpeningToken = openingToken;
    }

    @Override
    public boolean isClosingToken() {
        return isClosingToken;
    }

    public void setClosingToken(boolean closingToken) {
        isClosingToken = closingToken;
    }

}
