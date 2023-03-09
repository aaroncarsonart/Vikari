package com.atonement.crystals.dnr.vikari.core.operator.curriculum;

import com.atonement.crystals.dnr.vikari.core.AtonementCrystal;
import com.atonement.crystals.dnr.vikari.core.identifier.DefaultIdentifierMapping;

public class StudentOfStudentsCrystal extends AtonementCrystal {
    public StudentOfStudentsCrystal() {
        super(DefaultIdentifierMapping.STUDENT_OF_STUDENTS.getIdentifier());
    }
}