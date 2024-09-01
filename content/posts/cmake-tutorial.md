---
title: "CMake"
author: ["Karl Stump"]
date: 2024-08-31
tags: ["make"]
draft: false
---

Working through the CMake tutorial: <https://cmake.org/cmake/help/latest/guide/tutorial/index.html>


## Step 1 Exercise 1 {#step-1-exercise-1}

The first step (Step1) and this is the directory
stucture:

{{< highlight text >}}
Step1
├── CMakeLists.txt
├── CMakeLists.txt~
├── TutorialConfig.h.in
└── tutorial.cxx
{{< /highlight >}}

A minumum requirement for CMake, in CMakeLists.txt is the following.

1.  Required version
    1.  `cmake_minimum_required(VERSION 3.23)`
2.  Project name and version
    1.  project(Tutorial VERSION 1.0)
3.  Executable
    1.  `add_executable(Tutorial tutorial.cxx)`

So, for this tutorial this becomes:

{{< highlight text >}}
# TODO 1: Set the minimum required version of CMake to be 3.10
cmake_minimum_required(VERSION 3.23)# TODO

# TODO 2: Create a project named Tutorial
project(Tutorial VERSION 1.0)


# TODO 3: Add an executable called Tutorial to the project
# Hint: Be sure to specify the source file as tutorial.cxx
add_executable(Tutorial tutorial.cxx)
{{< /highlight >}}

Once that's done, you're instructed to create a build directory
mkdir `Step1_build`

{{< highlight bash >}}
mkdir Step1_build
cd Step1_build
# configure the project and generate a native build system
cmake ../Step1
# now, do the build
cmake --build .
{{< /highlight >}}

Here's the output from `cmake ../Step1`

{{< highlight bash >}}
cmake ../Step1
{{< /highlight >}}

Now, let's do the build

{{< highlight bash >}}
cmake --build .
{{< /highlight >}}

{{< highlight bash >}}
ls
{{< /highlight >}}

So for a very simple project, this is very easy to set up. To review you need three things
in the `CMakeLists.txt` file:

1.  The version of CMake
    -   Example: `cmake_minimum_required(VERSION 3.23)`
2.  The name of the project
    -   Example: project(Tutorial VERSION 1.0)
3.  The executable
    -   Example: `add_executable(Tutorial tutorial.cxx)`

Then creating a build directory somewhere, you need to do two things:

1.  run cmake on your source directory
    -   Example: cmake ../Step1
        -   This will generate the files needed for actually building
2.  Do the build
    -   Example: cmake &#x2013;build .

What the CMakeLists.txt file looks like:

{{< highlight text >}}
cmake_minimum_required(VERSION 3.23)

project(Tutorial VERSION 1.0)

add_executable(Tutorial tutorial.cxx)
{{< /highlight >}}

Bare bones, but it works, and it's perfect for getting things going!


## Step 1 Exercise 2 {#step-1-exercise-2}

In this exercise the goal was to add features that required C++11.

And this meant that CMakeLists.txt had to be modified:

{{< highlight text >}}
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
{{< /highlight >}}

Easy peasy.


## Step 1 Exercise 3 {#step-1-exercise-3}

Next comes adding a version number and configured header file.

How can you put  a version number in your CMakeLists.txt file and get that number
into your source code. CMake provides a mechanism for doing that using a configured header file.

First add the version number to CMakeLists.txt like this:

{{< highlight text >}}
project(Tutorial VERSION 1.0)
set(Tutorial_VERSION_MAJOR 23)
set(Tutorial_VERSION_MINOR 42)
{{< /highlight >}}

Notice that `project(Tutorial VERSION 1.0)` was added at the very beginning of theses exercises.  It
is one of the "must have" in your CMakeLists.txt file. Now, you can see part of the importance of
naming the project. CMake creates variables with that name, like `Tutorial_VERSION_MAJOR` and
`Tutorial_VERSION_MINOR`. So, you can set those. But there still has to be a way to get these numbers
into your program.

This is where the configured header file comes in. You can tell CMake to use some file (call
it an "input template" file) and make substitutions based on that file, and then to output
the results somewhere &#x2013; like into an `.h` file. This file can then be included by your program
and these definitions used. So, start with CMake's "configure file" in CMakeLists.txt

{{< highlight text >}}
configure_file(TutorialConfig.h.in TutorialConfig.h)
{{< /highlight >}}

The first file, TutorialConfig.h.in is the file that CMake is going to use like a template to
create TutorialConfig.h which will be included by your program. Let's look at TutorialConfig.h.in

{{< highlight text >}}
// the configured options and settings for Tutorial
// TODO 10: Define Tutorial_VERSION_MAJOR and Tutorial_VERSION_MINOR
#define Tutorial_VERSION_MAJOR @Tutorial_VERSION_MAJOR@
#define Tutorial_VERSION_MINOR @Tutorial_VERSION_MINOR@
{{< /highlight >}}

You can see that the definitions defined in CMakeLists.txt can be used between the `@'s`

Now, this creates `TutorialConfig.h` which, by the way, lives in the build directory.

{{< highlight text >}}
// the configured options and settings for Tutorial
// TODO 10: Define Tutorial_VERSION_MAJOR and Tutorial_VERSION_MINOR
#define Tutorial_VERSION_MAJOR 23
#define Tutorial_VERSION_MINOR 42
{{< /highlight >}}

So, this is an include file that your source code will include. Therefore you must tell CMake about this include
so the compiler can find it.

{{< highlight text >}}
target_include_directories(Tutorial PUBLIC "${PROJECT_BINARY_DIR}")
{{< /highlight >}}

This from the documents:

> The INTERFACE, PUBLIC and PRIVATE keywords are required to specify the scope of the following
> arguments. PRIVATE and PUBLIC items will populate the INCLUDE<sub>DIRECTORIES</sub> property of
> &lt;target&gt;. PUBLIC and INTERFACE items will populate the INTERFACE<sub>INCLUDE</sub><sub>DIRECTORIES</sub> property of
> &lt;target&gt;.

We haven't covered INTERFACE, PUBLIC, PRIVATE, but there they are.

This also from the documents explains "project binary directory."

> Since the configured file will be written into the project binary directory, we must add that
> directory to the list of paths to search for include files.
>
> Note: Throughout this tutorial, we will refer to the project build and the project binary directory
> interchangeably. These are the same and are not meant to refer to a bin/ directory

Now, in the source code we include the TutorialConfig.h and use it.

So, here is what our CMakeLists.txt looks like after all the exercises in Step 1:

{{< highlight text >}}

cmake_minimum_required(VERSION 3.23)

project(Tutorial VERSION 1.0)
set(Tutorial_VERSION_MAJOR 23)
set(Tutorial_VERSION_MINOR 42)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

configure_file(TutorialConfig.h.in TutorialConfig.h)

add_executable(Tutorial tutorial.cxx)

target_include_directories(Tutorial PUBLIC "${PROJECT_BINARY_DIR}")
{{< /highlight >}}


## Step 2 Exercise 1 {#step-2-exercise-1}

Adding a library. This is great &#x2014; In step 1 we've learned how to get things going. Now, by
adding libraries we will get into great organization ability.

As the tutorial says we can organize our project into one or more subdirectories.

Now, for myself, I can really neglect organization in my projects. So this is great.

The tutorial tells you that you'll use `add_library()` and `add_subdirectory()` commands, as well as
`target_include_directories()` and `target_link_libraries()`. All of these make sense right off the
bat.

What's interesting is that in the subdirectory for our libary and we'll put an additional CMakeLists.txt
file in there. Super!

Here's what Step2 looks like:

{{< highlight text >}}
Step2
├── CMakeLists.txt
├── MathFunctions
│   ├── CMakeLists.txt
│   ├── MathFunctions.cxx
│   ├── MathFunctions.h
│   ├── mysqrt.cxx
│   └── mysqrt.h
├── TutorialConfig.h.in
└── tutorial.cxx
{{< /highlight >}}

The first thing we have to do is create a build directory, and then in that directory
`configure` and then `build` the project. Three steps.

This will undoubtedly be the first thing we have to do in every tutorial step. And
we've already done it in step one.

So, `mkdir Step2_build` is the tutorial's preferred build directory. Then from that
directory, `cmake ../Step2` and then `cmake --build .` Don't forget, that the first
command `cmake ../Step2` is the configuration. And the results of that generation
are put into the build directory. Once that's done, you `build` in the build
directory.

By the way. CMake has a GUI. I haven't used it. But eventually I will. Is the command a better
approach to learning? I think so. It's great to know the details of what's happening (and go through
the **interminable** steps). Sometimes a gui can hide too much. At least for learning. So, command line
it is!

{{< highlight text >}}
mkdir Step2_build
cd Step2_build
cmake ../Step2
cmake --build .
{{< /highlight >}}

And the output as usual:

{{< highlight text >}}
kes@Zork:$ cmake ../Step2
-- The C compiler identification is GNU 13.2.0
-- The CXX compiler identification is GNU 13.2.0
-- Detecting C compiler ABI info
-- Detecting C compiler ABI info - done
-- Check for working C compiler: /usr/bin/cc - skipped
-- Detecting C compile features
-- Detecting C compile features - done
-- Detecting CXX compiler ABI info
-- Detecting CXX compiler ABI info - done
-- Check for working CXX compiler: /usr/bin/c++ - skipped
-- Detecting CXX compile features
-- Detecting CXX compile features - done
-- Configuring done
-- Generating done
-- Build files have been written to: /home/kes/Downloads/cmake/tutorial/Step2_build
kes@Zork:$cmake --build .
[ 50%] Building CXX object CMakeFiles/Tutorial.dir/tutorial.cxx.o
[100%] Linking CXX executable Tutorial
[100%] Built target Tutorial
kes@Zork:~/Downloads/cmake/tutorial/Step2_build$
{{< /highlight >}}

Tastes great! Lasts a long time!


### Add Library {#add-library}

So the first thing we have to do is add the library in the `Mathfunctions/CMakeLists.txt`

The documents say:

> `add_library(<name> [<type>] [EXCLUDE_FROM_ALL] <sources>...)`

Of course, nothing is easy. What should the name be after all? The documents are helpful:

> The &lt;name&gt; corresponds to the logical target name and must be globally unique within a project. The
> actual file name of the library built is constructed based on conventions of the native platform
> (such as lib&lt;name&gt;.a or &lt;name&gt;.lib)

Based on the name of the subdirectory, and the capitalization of the files, the name wanted is
either MathFunctions or mathfunctions.

For &lt;type&gt; there are three options, but three options, `STATIC`, `SHARED`, and `MODULE`. Of course,
`TYPE` is between brackets, so it's optional. And the documentation
tells us:

> If no &lt;type&gt; is given the default is STATIC or SHARED based on the value of the `BUILD_SHARED_LIBS`
> variable.

The tutorial hasn't covered `BUILD_SHARED_LIBS`, and it's probably safe to skip type. And
`EXCLUDE_FROM_ALL` has not been covered either. So, skip it too.

So, this leaves the library logical name and the source files:

{{< highlight text >}}
add_library(MathFunctions MathFunctions.cxx mysqrt.cxx)
{{< /highlight >}}

Now, we have to edit the top level CMakeLists.txt file. We'll need to add the subdirectory, and the
`target_link_libraries`. The documents helpfull define the `target_link_libraries` command like this:

> `target_link_libraries(<target> <PRIVATE|PUBLIC|INTERFACE> <item>...
>                               [<PRIVATE|PUBLIC|INTERFACE> <item>...]...)`

So, in the top-level CMakeLists.txt file we add:

{{< highlight text >}}
add_subdirectory(MathFunctions)
target_link_libraries(Tutorial PUBLIC MathFunctions)
{{< /highlight >}}

And we need to specifiy where the header files live for this library using `target_include_directories` &#x2013;
we did that in Step1, too, for the configured header. Now, we're adding to it, note the difference
between `${PROJECT_BINARY_DIR}"` and `${PROJECT_SOURCE_DIR}/MathFunctions"`

{{< highlight text >}}
target_include_directories(Tutorial PUBLIC
                           "${PROJECT_BINARY_DIR}"
			   "${PROJECT_SOURCE_DIR}/MathFunctions"
                           )
{{< /highlight >}}

{{< highlight text >}}
kes@Zork:~/Downloads/cmake/tutorial/Step2_build$ cmake --build .
-- Configuring done
-- Generating done
-- Build files have been written to: /home/kes/Downloads/cmake/tutorial/Step2_build
Consolidate compiler generated dependencies of target MathFunctions
[ 60%] Built target MathFunctions
Consolidate compiler generated dependencies of target Tutorial
[ 80%] Building CXX object CMakeFiles/Tutorial.dir/tutorial.cxx.o
[100%] Linking CXX executable Tutorial
[100%] Built target Tutorial
kes@Zork:~/Downloads/cmake/tutorial/Step2_build$
{{< /highlight >}}

Okay, so let's summarize.

1.  We started off Step2 in the same way Step1
    -   Make the build directory
    -   enter the build directory and configure the project (`cmake ../Step2`)
    -   Then build the project (`cmake --build .`)
2.  Then, for the first exercise we added a library. So, what's required?
    -   The library directory was already there: `MathFunctions`
    -   In the library directory there was already a CMakeLists.txt file. But we must specify the
        library name and the source files with the `add_library` command.
    -   Next we must modify the top-level CMakeLists.txt file
        -   We have to tell CMake that there's a subdirectory. This we did with the conveiniently named
            `add_subdirectory` command.
        -   Now we have to tell it about the target link libraries `target_link_libraries`
        -   And we have to tell it about the target include directories with `target_include_directories`
            command &#x2014; and we note the difference between, `"${PROJECT_BINARY_DIR}"` and `"${PROJECT_SOURCE_DIR}`

And that's it!

Very cool!
