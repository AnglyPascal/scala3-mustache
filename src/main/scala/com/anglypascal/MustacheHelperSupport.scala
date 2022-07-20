package com.anglypascal.mustache

trait MustacheHelperSupport:
  type Render = String => String
  private val contextLocal = new java.lang.ThreadLocal[Any]()
  private val renderLocal  = new java.lang.ThreadLocal[Render]()

  protected def context: Any   = contextLocal.get
  protected def render: Render = renderLocal.get()(_)

  def withContextAndRenderFn[A](context: Any, render: Render)(fn: => A): A =
    contextLocal.set(context)
    renderLocal.set(render)
    try fn
    finally
      contextLocal.set(null)
      renderLocal.set(null)
