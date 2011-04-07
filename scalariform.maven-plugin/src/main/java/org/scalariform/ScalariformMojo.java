package org.scalariform;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

/**
 * Goal which formats scala source files
 *
 * @goal format
 * 
 * @phase process-sources
 */
public class ScalariformMojo extends AbstractMojo {

    /**
     * Base directory of the project
     *
     * @parameter expression="${basedir}"
     * @required
     */
    protected String baseDir;

    /**
     *  @parameter default-value=false
     */
    protected boolean alignParameters;

    /**
     *  @parameter default-value=false
     */
    protected boolean alignSingleLineCaseStatements;

    /**
     *  @parameter default-value=40
     */
    protected int alignSingleLineCaseStatements_maxArrowIndent;

    /**
     *  @parameter default-value=false
     */
    protected boolean compactStringConcatenation;

    /**
     *  @parameter default-value=true
     */
    protected boolean doubleIndentClassDeclaration;

    /**
     *  @parameter default-value=true
     */
    protected boolean formatXml;

    /**
     *  @parameter default-value=false
     */
    protected boolean indentLocalDefs;

    /**
     *  @parameter default-value=true
     */
    protected boolean indentPackageBlocks;

    /**
     *  @parameter default-value=2
     */
    protected int indentSpaces;

    /**
     *  @parameter default-value=false
     */
    protected int multilineScaladocCommentsStartOnFirstLine;

    /**
     *  @parameter default-value=false
     */
    protected boolean preserveDanglingCloseParenthesis;

    /**
     *  @parameter default-value=false
     */
    protected boolean preserveSpaceBeforeArguments;

    /**
     *  @parameter default-value=false
     */
    protected boolean rewriteArrowSymbols;

    /**
     *  @parameter default-value=false
     */
    protected boolean spaceBeforeColon;

    /**
     *  @parameter default-value=false
     */
    protected boolean spacesInsideBrackets;

    /**
     *  @parameter default-value=false
     */
    protected boolean spacesInsideParentheses;
    
    /**
     *  @parameter default-value=true
     */
    protected boolean spacesWithinPatternBinders;

    public void execute() throws MojoExecutionException {

	MojoFormatter.format(baseDir, this.getLog(),
                             alignParameters, 
                             alignSingleLineCaseStatements,
                             alignSingleLineCaseStatements_maxArrowIndent,
                             compactStringConcatenation,
                             doubleIndentClassDeclaration,
                             formatXml,
                             indentLocalDefs,
                             indentPackageBlocks,
                             multilineScaladocCommentsStartOnFirstLine,
                             preserveDanglingCloseParenthesis,
                             preserveSpaceBeforeArguments,
                             rewriteArrowSymbols,
                             spaceBeforeColon,
                             spacesInsideBrackets,
                             spacesInsideParentheses,
                             spacesWithinPatternBinders,
                             indentSpaces);
    }

}

