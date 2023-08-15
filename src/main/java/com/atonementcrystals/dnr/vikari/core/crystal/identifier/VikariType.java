package com.atonementcrystals.dnr.vikari.core.crystal.identifier;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.TypeHierarchy;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.BooleanCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.literal.NullCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigDecimalCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.BigIntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.DoubleCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.FloatCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.IntegerCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.LongCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.number.NumberCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.value.ValueCrystal;
import com.atonementcrystals.dnr.vikari.util.Utils;

import java.util.Arrays;
import java.util.EnumSet;
import java.util.stream.Collectors;

/**
 * Holds state for the subset of types for crystals which are instantiable
 * (supported for ReferenceCrystals) and are built-in to Vikari. Custom
 * Types defined with a Type declaration in a user's program will be
 * handled separately from this class.
 */
public enum VikariType {
    ATONEMENT_CRYSTAL(AtonementCrystal.class),
    NULL(NullCrystal.class),
    INVALID(null, "InvalidType"),

    // base types
    TYPE(TypeCrystal.class),
    VALUE(ValueCrystal.class),

    // numeric types
    NUMBER(NumberCrystal.class),
    INTEGER(IntegerCrystal.class),
    LONG(LongCrystal.class),
    BIG_INTEGER(BigIntegerCrystal.class),
    FLOAT(FloatCrystal.class),
    DOUBLE(DoubleCrystal.class),
    BIG_DECIMAL(BigDecimalCrystal.class),

    // other types
    BOOLEAN(BooleanCrystal.class);

    public static final String LANG_PACKAGE = "dnr::vikari::lang";
    public static final EnumSet<VikariType> LANG_TYPES = initLangTypes();

    // TODO: Add other sets when other base types such as Records, Enums etc are added.
    public static final EnumSet<VikariType> TYPE_TYPES = initTypeTypes();

    /**
     * @return The set of all types declared in the lang package.
     */
    public static EnumSet<VikariType> initLangTypes() {
        return Arrays.stream(values())
                .filter(vikariType -> vikariType.typeCrystal.getPackageName().equals(LANG_PACKAGE))
                .filter(vikariType -> vikariType != INVALID)
                .collect(Collectors.toCollection(() -> EnumSet.noneOf(VikariType.class)));
    }

    /**
     * @return The set of all types with type Type.
     */
    public static EnumSet<VikariType> initTypeTypes() {
        return EnumSet.of(ATONEMENT_CRYSTAL, NULL, INVALID, VALUE, TYPE, NUMBER, INTEGER, LONG, BIG_INTEGER, FLOAT,
                DOUBLE, BIG_DECIMAL, BOOLEAN);
    }

    /*
     * For each VikariType in TYPE_TYPES, initialize its typeCrystal's typeCrystal as TypeCrystal
     * for all Types defined in this enum. Also: Set up the type inheritance hierarchy.
     */
    static {
        for (VikariType vikariType : TYPE_TYPES) {
            TypeCrystal typeCrystal = vikariType.typeCrystal;
            typeCrystal.setType(VikariType.TYPE);
        }
        TypeHierarchy.init();
    }

    private final Class<? extends AtonementCrystal> javaType;
    private final TypeCrystal typeCrystal;

    VikariType(String packageName, Class<? extends AtonementCrystal> javaType, String typeName) {
        this.javaType = javaType;
        this.typeCrystal = new TypeCrystal(packageName, typeName);
    }

    VikariType(Class<? extends AtonementCrystal> javaType, String typeName) {
        this(LANG_PACKAGE, javaType, typeName);
    }

    VikariType(Class<? extends AtonementCrystal> javaType) {
        this(LANG_PACKAGE, javaType, Utils.getSimpleClassName(javaType));
    }

    public Class<? extends AtonementCrystal> getJavaType() {
        return javaType;
    }

    public TypeCrystal getTypeCrystal() {
        return typeCrystal;
    }
}
