package org.scalariform;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

import org.apache.maven.project.MavenProject;

import scalariform.formatter.preferences.*;
import scalariform.formatter.ScalaFormatter;
import scalariform.parser.ScalaParserException;

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
    protected boolean compactStringConcatenation;

    /**
     *  @parameter default-value=true
     */
    protected boolean doubleIndentClassDeclaration;

    /**
     *  @parameter default-value=2
     */
    protected int indentSpaces;

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

    
    public void execute() throws MojoExecutionException {

	MojoFormatter.format(baseDir, 
                             alignParameters, 
                             compactStringConcatenation,
                             doubleIndentClassDeclaration,
                             preserveSpaceBeforeArguments,
                             rewriteArrowSymbols,
                             spaceBeforeColon,
                             indentSpaces);
    }

}

