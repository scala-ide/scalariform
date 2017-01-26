resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.8")

resolvers += Classpaths.typesafeSnapshots

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

retrieveManaged := true

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")
