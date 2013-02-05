# hRunner

Runner dialog for starting applications, searching the web and
calculating arithmetic expressions.

It is made to be small, fast and simple with a minimum of dependencies.

Oh, and it's implemented in Haskell.


## How to use

  1. Build it and put the executable somewhere

  2. Run it to create a `.hrunner` directory in `$HOME` (on Windows this
     will go somewhere else)
  
  3. Edit `.hrunner/shortcuts` to your liking. More on this later...

  4. Create a desktop shortcut to the hrunner executable
     (I use Win+R in xfce and Ctrl+Alt+P in Gnome3)

  5. Start using! Hit your shortcut and start running things.


## Features

  * Autocomple (of previous commands)

  * Evaluation of arithmetic expressions (using python, so any python
    expression would evaluate)

  * User configurable shortcuts


## Background

Several years ago I was running KDE 3.5 as my primary desktop. It had,
among many other great things, a very nice application starter
(accessible by hitting ALT+F2). Not only did it start applications,
but could also send text strings to web pages for searching. I don't
remember all other things it could do.

Then when KDE 4 came out I migrated to XFCE. Although being a much
simpler desktop environment it still served my needs (and still does)
pretty well. But the one thing I missed the most from KDE was the
application starter.

This is how hrunner came to be. XFCE also has an application runner,
but it cannot search the web, and at the time it didn't even have auto
completion.


## And here it is

I wrote hrunner for my own needs with no thought of making it publicly
available. But now I have been using it for some years and it serves
me very well, so why not make it available for anyone.

It is still in need of polish and the code is rather old. But it has a
very small code base, few dependencies and hence should be rather
simple to hack on for those who like.


## TODO

  * Add example shortcuts file(s)
