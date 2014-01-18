+ Parameters now not only align by the first token in a parameter,
  but also by the type and any defaults in the parameter

  Original Source, before running scalariform
  ```scala
    def showInput[A](
    parent: Component = null,
    message: Any,
    title: String = uiString("OptionPane.inputDialogTitle"),
    messageType: Message.Value = Message.Question,
    icon: Icon = EmptyIcon,
    entries: Seq[A] = Nil,
    initial: A): Option[A]
  ```

  Before this update, after running scalariform
  ```scala
    def showInput[A](
      parent: Component = null,
      message: Any,
      title: String = uiString("OptionPane.inputDialogTitle"),
      messageType: Message.Value = Message.Question,
      icon: Icon = EmptyIcon,
      entries: Seq[A] = Nil,
      initial: A): Option[A]
  ```

  After this update, after running scalariform
  ```scala
    def showInput[A](
      parent:      Component     = null,
      message:     Any,
      title:       String        = uiString("OptionPane.inputDialogTitle"),
      messageType: Message.Value = Message.Question,
      icon:        Icon          = EmptyIcon,
      entries:     Seq[A]        = Nil,
      initial:     A): Option[A]
  ```

+ The `implicit` modifier is now placed on it's own line in parameters if AlignParameters is enabled (and there's a newline in the parameter)

  Original Source, before running scalariform
  ```scala
    def(
      arg1: String
    )(
      implicit arg2: ExecutionContext = Akka.system,
      arg3: SomeImplicitableType
    )
  ```

  Before this update, after running scalariform
  ```scala
    def(
      arg1: String)(
        implicit arg2: ExecutionContext = Akka.system,
        arg3: SomeImplicitableType)
  ```

  After this update, after running scalariform
  ```scala
    def(
      arg1: String)(
        implicit
        arg2: ExecutionContext     = Akka.system,
        arg3: SomeImplicitableType)
  ```

+ Multi-line types will not be placed on a new line unless the original source
  parameter started with a newline

  Original Source, before running scalariform
  ```scala
    class A(a: Int,
    b: Int)(c: { val d: Int
    })
  ```

  Before this update, after running scalariform
  ```scala
    class A(a: Int,
            b: Int)(
              c: {
                val d: Int
              })
  ```

  After this update, after running scalariform
  ```scala
    class A(a: Int,
            b: Int)(c: {
                      val d: Int
                    })
  ```