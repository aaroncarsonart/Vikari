package com.atonementcrystals.dnr.vikari.core.crystal.separator;

import com.atonementcrystals.dnr.vikari.core.crystal.AtonementCrystal;
import com.atonementcrystals.dnr.vikari.core.crystal.identifier.TokenType;

/**
 * The region operator :: either declares a new code region, or dereferences
 * an existing module, package, or region. Region declarations must either
 * be a series of statements at one increased level of indentation, or a
 * single line of comma-separated statements terminated by either a newline
 * or a semicolon.<br/>
 * <pre>
 * ~:Config region.:~
 * import ::
 *   vikari::util::Pair      ~:Import a type.:~
 *   vikari::util::Math::abs ~:Import a static function.:~
 *
 * ~:Control-flow region, containing two statements defined on one line.:~
 * ?? [foo = bar] :: foo.update!(), :baz: ;
 *
 * ~:Function region.:~
 * compareMagnitude << (x:Number|y:Number): Boolean ::
 *   ^^ abs!(x) < abs!(y)</pre>
 *
 * There are many kinds of code regions: control flow regions, function
 * regions, type regions, field regions, and config regions. Function, type,
 * and field regions declare a new environment which allows name shadowing,
 * while environments of control flow regions do not.<br/>
 * <br/>
 * Config regions include the package, import, and inherits regions. These
 * regions do not establish a new environment, but rather configure the
 * current file or type, respectively. The package region declares the
 * relative path to the source file from the project root. The import region
 * accept a series of fully-qualified references to establish what additional
 * names are available in the root environment. While the import region
 * declares a list of type names that a given type inherits from within its
 * type declaration.<br/>
 * <br/>
 * Module, package, and region names are dereferenced with :: in the same
 * manner as static field members are dereferenced from a crystal instance.<br/>
 * <pre>
 * ModuleName::packageName::TypeName::regionName::functionName</pre>
 *
 * Only static field members can be imported from a Type.
 */
public class RegionOperatorCrystal extends AtonementCrystal {

    public RegionOperatorCrystal() {
        super(TokenType.REGION_OPERATOR.getIdentifier());
    }

}