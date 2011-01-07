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
     */
    protected String baseDir;

    public void execute() throws MojoExecutionException {
	MojoFormatter.format(baseDir);
    }

}

