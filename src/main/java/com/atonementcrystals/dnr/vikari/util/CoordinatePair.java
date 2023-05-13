package com.atonementcrystals.dnr.vikari.util;

/**
 * Represents a row and column coordinate pair. Used for locating the
 * exact position of a crystal's identifier within a Vikari source file.
 */
public class CoordinatePair implements Comparable<CoordinatePair> {

    private final int row;
    private final int column;

    public CoordinatePair(int row, int column) {
        this.row = row;
        this.column = column;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof CoordinatePair) {
            CoordinatePair that = (CoordinatePair) obj;
            return this.row == that.row &&
                    this.column == that.column;
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 17;
        hash = hash * 31 + row;
        hash = hash * 31 + column;
        return hash;
    }

    @Override
    public String toString() {
        return "CoordinatePair(row=" + row + ",column=" + column + ")";
    }

    @Override
    public int compareTo(CoordinatePair that) {
        int rowResult = Integer.compare(this.row, that.row);
        if (rowResult == 0) {
            return Integer.compare(this.column, that.column);
        }
        return rowResult;
    }
}
