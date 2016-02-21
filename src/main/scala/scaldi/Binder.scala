package scaldi

import language.postfixOps

import scaldi.util.Util._
import scala.reflect.runtime.universe.{TypeTag, Type, typeTag}
import scaldi.util.ReflectionHelper

/**
  * Provides binding DSL in body of the subclasses.
  */
trait WordBinder {
  private var bindingsInProgress: List[BindHelper[_]] = Nil
  private var bindings: List[BoundHelper[_]] = Nil
  private var contextCondition: Option[() => Condition] = None

  protected def injector: Injector

  /**
    * Provides current bindings, including the binding to the injector
    * Throws exception if there are still bindings in progress
    */
  lazy val wordBindings: List[BindingWithLifecycle] = {
    if (bindingsInProgress nonEmpty) {
      throw new BindingException(
          bindingsInProgress
              .map(b => "\tBinding with identifiers: " + (b.identifiers mkString ", "))
              .mkString("Following bindings are not bound to anything (please use 'to', 'toProvider' or 'toNonLazy']):\n","\n", "")
      )
    }

    val injectorBinding = ProviderBinding(() => injector, List(TypeTagIdentifier.typeId[Injector]))

    injectorBinding :: bindings.map(_ getBinding).reverse
  }

  /**
    * Creates a binding, doesn't take any type parameters as the type of
    * the binding will be the inferred type of the binded value
    * @return a new dummy binding
    */
  def binding = createBinding[Any](None, contextCondition)

  /**
    * Creates a binding to the type provided in type parameter.
    * @tparam T type to which the binding will be associated
    * @return a new binding
    */
  def bind[T : TypeTag] = createBinding[T](Some(typeTag[T]), contextCondition)

  /**
    * Provides a possibility to define conditions for bindings.
    * Usefull when having many binding for the same type,
    * injected depending on some condition
    *
    * {{{
    *   bind [Database] when inProdMode to new Riak
    *   bind [Database] when (inDevMode or inTestMode) to new InMemoryDatabase
    * }}}
    * @param condition condition on which the binding will be chosen
    * @param fn ???
    */
  def when(condition: => Condition)(fn: => Unit) = {
    contextCondition = contextCondition map (c => () => condition and c()) orElse Some(() => condition)
    fn
    contextCondition = None
  }

  /**
    * Marks `Identifier` as required i.e. the binding won't be injected
    * if it was not identified by a required `Identifier`.
    * Without required bindings are injected in order of definition (latest
    * defined binding has a priority)
    * @param identifier `Identifier` that will be required to inject the binding
    * @return a required `Identifier`
    */
  def required(identifier: Identifier): Identifier = RequiredIdentifier(identifier, isRequired = true)

  /**
    * Marks `Identifier` as not required i.e. the binding will be injected
    * even if the identifier is not specified
    * @param identifier `Identifier` that will not be required for the injection
    * @return a non-required `Identifier`
    */
  def notRequired(identifier: Identifier): Identifier = RequiredIdentifier(identifier, isRequired = false)

  private def createBinding[T](mainType: Option[TypeTag[_]], condition: Option[() => Condition]) = {
    val helper = new BindHelper[T]({ (bind, bound) =>
      bindingsInProgress = bindingsInProgress filterNot (bind ==)
      bindings = bindings :+ bound
    })

    bindingsInProgress = bindingsInProgress :+ helper

    mainType foreach (helper identifiedBy _)
    condition foreach (c => helper when c())

    helper
  }

  /**
    * Initializes bindings that are not lazy
    * @param lifecycleManager lyfecycle manager that will be responsible for binding initialization
    * @return initialization function
    */
  protected def initEagerWordBindings(lifecycleManager: LifecycleManager): () => Unit =
    wordBindings |> (b => () => b.filter(_.isEager).foreach(_ init lifecycleManager))
}

/**
  * Provides DSL to make binding identifiable
  * @tparam R ???
  */
trait CanBeIdentified[R] { this: R =>
  /**
    * Current list of identifiers attached to the binding
    */
  var identifiers : List[Identifier] = Nil

  /**
    * Appends an `Identifier` to a list of binding's identifiers
    * @param ids vargs of identifiers
    * @return ???
    */
  def identifiedBy(ids: Identifier*): R = {
    identifiers = identifiers ++ ids
    this
  }

  /**
    * Alias to `identifiedBy`
    * @param ids vargs of identifiers
    * @return ???
    */
  def as(ids: Identifier*): R = identifiedBy(ids: _*)

  /**
    * DSL to add more than one identifier to a binding
    * @param ids vargs of identifiers
    * @return ???
    */
  def and(ids: Identifier*): R = identifiedBy(ids: _*)
}

/**
  * Provides DSL to make binding conditional
  * @tparam R ???
  */
trait CanBeConditional[R] { this: R =>
  /**
    * Current binding's conditions
    */
  var condition: Option[() => Condition] = None

  /**
    * Appends condition to current contidions
    * @param cond `Condition` to append
    */
  def when(cond: => Condition) = {
    condition = condition map (c => () => cond and c()) orElse Some(() => cond)
    this
  }
}

/**
  * Provides DSL to add life cycle to a binding
  * @tparam H ???
  * @tparam D ???
  */
trait CanHaveLifecycle[H, D] { this: H =>
  /**
    * Current binding's lifecycle
    */
  var lifecycle = BindingLifecycle.empty[D]

  /**
    * Modifies life cycle initialization function
    * @param initFn function that will be set for binding initialization
    */
  def initWith(initFn: (D) => Unit) = {
    lifecycle = lifecycle.copy(initialize = Some(initFn))
    this
  }

  /**
    * Modifies life cycle destruction function
    * @param destroyFn function that will be set for binding destruction
    */
  def destroyWith(destroyFn: (D) => Unit) = {
    lifecycle = lifecycle.copy(destroy = Some(destroyFn))
    this
  }
}

/**
  * ???
  * @param bindingFn ???
  * @tparam T ???
  */
case class WordBindingProvider[T](bindingFn: (List[Identifier], Option[() => Condition], BindingLifecycle[Any]) => BindingWithLifecycle)

/**
  * Used to initialize a binding
  * @param onBound ???
  * @tparam R ???
  */
class BindHelper[R](onBound: (BindHelper[R], BoundHelper[_]) => Unit)
    extends CanBeIdentified[BindHelper[R]] with CanBeConditional[BindHelper[R]] {
  /**
    * Current binding's creation function
    */
  var createFn: Option[Option[() => Any]] = None

  /**
    * Used to undefine the binding
    * @param none `None`
    */
  def to(none: None.type) = bindNone[R](LazyBinding(None, _, _, _))

  /**
    * Defines a lazy binding ???
    * @param provider ???
    * @tparam T ???
    * @return ???
    */
  def to[T <: R : TypeTag](provider: WordBindingProvider[T]) = bind(provider.bindingFn)

  /**
    * Defines a lazy binding ???
    * @param fn ???
    * @tparam T ???
    * @return ???
    */
  def to[T <: R : TypeTag](fn: => T) = bind(LazyBinding(Some(() => fn), _, _, _))

  @deprecated("`in` variant is deprecated in favor of `to` syntax", "0.5")
  def in[T <: R : TypeTag](fn: => T) = to(fn)

  /**
    * Initializes a non-lazy binding, the binding value will be created only once,
    * but it will be created as soon as the injector is initialized.
    * @param fn ???
    * @tparam T ???
    * @return ???
    */
  def toNonLazy[T <: R : TypeTag](fn: => T) = bind(NonLazyBinding(Some(() => fn), _, _, _))

  @deprecated("`in` variant is deprecated in favor of `to` syntax", "0.5")
  def inNonLazy[T <: R : TypeTag](fn: => T) = toNonLazy(fn)

  /**
    * Initializes a provider binding, the binding value will be re-created each
    * time you inject the binding.
    * @param fn ???
    * @tparam T ???
    * @return ???
    */
  def toProvider[T <: R : TypeTag](fn: => T) = bind(ProviderBinding(() => fn, _, _, _))

  @deprecated("`in` variant is deprecated in favor of `to` syntax", "0.5")
  def inProvider[T <: R : TypeTag](fn: => T) = toProvider(fn)

  private def bind[T : TypeTag](bindingFn: (List[Identifier], Option[() => Condition], BindingLifecycle[Any]) => BindingWithLifecycle) = {
    val bound = new BoundHelper[T](bindingFn, identifiers, condition, Some(typeTag[T].tpe))
    onBound(this, bound)
    bound
  }

  private def bindNone[D](bindingFn: (List[Identifier], Option[() => Condition], BindingLifecycle[Any]) => BindingWithLifecycle) = {
    val bound = new BoundHelper[D](bindingFn, identifiers, condition, None)
    onBound(this, bound)
    bound
  }
}

/**
  * Used to initialize binding
  * @param bindingFn ???
  * @param initialIdentifiers ???
  * @param initialCondition ???
  * @param bindingType ???
  * @tparam D ???
  */
class BoundHelper[D](
   bindingFn: (List[Identifier], Option[() => Condition], BindingLifecycle[Any]) => BindingWithLifecycle,
   initialIdentifiers: List[Identifier],
   initialCondition: Option[() => Condition],
   bindingType: Option[Type]
) extends CanBeIdentified[BoundHelper[D]] with CanBeConditional[BoundHelper[D]] with CanHaveLifecycle[BoundHelper[D], D] {
  /**
    * ???
    * @return ???
    */
  def getBinding = bindingFn (
    (initialIdentifiers ++ identifiers, bindingType) match {
      case (ids, _) if ids.exists(_.isInstanceOf[TypeTagIdentifier]) => ids
      case (ids, Some(t)) => ids :+ TypeTagIdentifier(t)
      case (ids, None) => ids
    },
    condition orElse initialCondition,
    lifecycle.asInstanceOf[BindingLifecycle[Any]]
  )
}

@deprecated("ReflectionBinder is deprecated and will be removed soon. As an alternative you can use `WordBinder` or create your own injector that is marked as `ImmutableInjector`.", "0.5")
trait ReflectionBinder {
  lazy val reflectiveBindings: List[Binding] = {
    import scala.reflect.runtime.universe._

    val mirror = ReflectionHelper.mirror
    val reflection = mirror reflect this

    // TODO: filter even more - all library, Scala and JDK methods should be somehow filtered!
    mirror.classSymbol(this.getClass).toType
      .members
      .filter(_.isPublic)
      .filter(_.isMethod)
      .filterNot(_.isMacro)
      .filterNot(_.isConstructor)
      .map(_.asMethod)
      .filterNot(_.returnType =:= typeOf[Nothing])
      .map { m =>
        if (m.returnType <:< typeOf[BindingProvider])
          reflection.reflectMethod(m).apply().asInstanceOf[BindingProvider].getBinding(m.name.decodedName.toString, m.returnType)
        else
          ReflectiveBinding(() => Some(reflection.reflectMethod(m).apply()), List(m.returnType, m.name.decodedName.toString))
      }
      .toList
  }

  case class ReflectiveBinding(fn: () => Option[Any], identifiers: List[Identifier]) extends Binding {
    val condition = None
    override def get = fn()
  }
}

@deprecated("BindingProvider is deprecated and will be removed soon. As an alternative you can use `ImmutableWrapper` injector to define an immutability boundary in composition or create your own injector that is marked as `ImmutableInjector`.", "0.5")
trait BindingProvider {
  def getBinding(name: String, tpe: Type): Binding
}

/**
  * Custom exception used during bindings
  * @param message exception's message
  * @param cause exception's cause
  */
class BindingException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this(message: String) = this(message, null)
}