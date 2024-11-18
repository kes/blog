---
title: "Emacs Modules"
author: ["Karl Stump"]
date: 2024-11-18
tags: ["emacs", "C", "programming", "lisp"]
draft: false
---

Emacs is implemented in elisp (most of it). Extending, modifying or customizing emacs is also done
in elisp. (However, some of emacs is written in C.),

As of Emacs 25 it is possible to extend emacs by writing modules in C.

Let's look at a simple example using the notes from <https://www.gnu.org/software/emacs/manual/html_node/elisp/Module-Initialization.html>


## Start the Module {#start-the-module}

A module **must** start with the following:

{{< highlight C >}}
#include <emacs-module.h>

int plugin_is_GPL_compatible;
{{< /highlight >}}

After that, it's pretty easy to create a test function. The steps are as follows:

1.  Define the function. In our case we'll just be returning an integer.
2.  Next, you need to provide a function called int `emacs_module_init(struct emacs_runtime *runtime)`
    &#x2013; more about this below.
3.  Finally, you'll need some way to "wire up" your function so that emacs can get to it. This is done
    by "binding," the function to a symbol, and "providing" the module.


## Initialization Function {#initialization-function}

An initialization function is needed. Emacs will call this function when emacs loads the module (i.e., when `(require 'module-name)` is executed). If the initialization does not exist, emacs generates an error. Note, this function should return 0 if all goes well, and non-zero if there's an error of some kind.

Okay, so we need:

`Function: int emacs_module_init (struct emacs_runtime *runtime)`

By the way, there are any number of tests that can be run on initialization, for example if you wanted to check for the emacs version. See details from the link provided. For our purposes, we simply move on to initializing the function that we want to to call from elisp.

The beginning code looks like this:

{{< highlight C >}}
int emacs_module_init (struct emacs_runtime *runtime){
  // You could put various checks and guards here .....

  // get the environment
  emacs_env *env = runtime->get_environment (runtime);
  // continues next section
{{< /highlight >}}

That's the beginning &#x2014; now we need to create the function pointer.


## Create Function Pointer {#create-function-pointer}

Creating the function point is easy.

{{< highlight C >}}
emacs_value func = env->make_function (env, 0, 0,
                                       (void *) my_get_number,
                                       "This function returens a number", NULL);
{{< /highlight >}}


## Bind and Provide {#bind-and-provide}

Now, we just bind and provide.

{{< highlight C >}}
/* bind */
emacs_value symbol = env->intern (env, "Fmymod_test");
emacs_value args[] = {symbol, func};

/* provide */
env->funcall (env, env->intern (env, "defalias"), 2, args);
{{< /highlight >}}


## The Entire Code {#the-entire-code}

As long as you keep the few steps in mind, the code is self-explanatory.

1.  Define the function you'll be calling from emacs
2.  Define your init function. In that function you will do the following:
    1.  Create a function pointer
    2.  Bind the function pointer to a symbol
    3.  Provide the module to emacs.

Here's the entire code:

{{< highlight C >}}
#include <emacs-module.h>
#include <assert.h>

int plugin_is_GPL_compatible;

/* New emacs lisp function. All function exposed to Emacs must have this prototype. */
static emacs_value my_get_number (emacs_env *env, int nargs, emacs_value args[], void *data){
  assert (nargs == 0);
  // return a value
  return env->make_integer (env, 9991);
}

/* helper function for binding */
static void bind_symbol_to_func (emacs_env *env, const char *func_name, emacs_value function){
  emacs_value fset = env->intern (env, "fset");
  emacs_value symbol = env->intern (env, func_name);
  emacs_value args[] = {symbol, function};

  // Lisp: *symbol ----> C: function (which is: Fmymod_test)
  env->funcall(env, fset, 2, args);

}
/* helper function for providing */
static void provide_module (emacs_env *env, const char *module){
  emacs_value interned_module = env->intern (env, module);
  emacs_value provide = env->intern (env, "provide");
  emacs_value args[] = { interned_module };
  env->funcall (env, provide, 1, args);
}


/* initialize -- which creates the function, binds, and provides it. */
int emacs_module_init (struct emacs_runtime *runtime){
  emacs_env *env = runtime->get_environment (runtime);

  emacs_value func = env->make_function (env, 0, 0,
                                         (void *) my_get_number,
                                         "This function returens a number", NULL);


  bind_symbol_to_func(env, "my-get-number", func);
  provide_module(env, "mymod");

  return 0;
}
{{< /highlight >}}

And here's a Makefile:

{{< highlight bash >}}
ROOT	= $HOME/repos/emacs/ # yours will be different
CC	= gcc
LD	= gcc
CFLAGS	= -ggdb3 -Wall
LDFLAGS =

all: mymod.so


%.so: %.o
        $(LD) -shared $(LDFLAGS) -o $@ $<

%.o: %.c
        $(CC) $(CFLAGS) -I$(ROOT)/src -fPIC -c $<
{{< /highlight >}}

Now, when you start emacs, in the **scratch** buffer you can run

{{< highlight elisp >}}
(require 'mymod)
(provide 'my-get-number)

(my-get-number)
{{< /highlight >}}

Have fun!
