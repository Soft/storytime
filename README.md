# Storytime

Storytime is a choice-based interactive fiction system. It features a simple
plain-text format for creating stories and has a support for conditional
execution and simple arithmetic.

## Install

~~~bash
hg clone ssh://hg@bitbucket.org/Soft/storytime
cd storytime
cabal install --only-dependencies
cabal build
cabal install --user
~~~

## Usage

~~~bash
storytime [-V|--validate] FILE
~~~

## Language

### Sections and Links

Sections are the backbone of Storytime documents and every piece of fiction must
have at least one of them. One can define a new section by starting the line
with an asterisk and following it with section's name. Section names have to
begin with a letter and can contain alphanumeric characters, hyphen (-), or
forward slash (/).

~~~
* beginning

Once in a far away land...

[castle]: Explore the castle
[invest]: Become a fairly successful middle manager at a mid-sized investment bank

* castle

You gaze upon the steep walls of the fortress. Climbing them would certainly be
a hurdle.

* invest

On a whim, you decide to look for lucrative investment opportunities in the
derivatives market.

~~~

Links connect the sections together and make the story come to live. Links have
to be listed after the content of a section. In the most basic form, a link
consists of a target and a title.

### Variables and Conditions

### Miscellanea

Storytime documents can start with a metadata block. How the metadata will be
used depends on the selected viewer.

~~~
% title: A Witty Title
% author: John Doe

* first-section

...
~~~

## Examples

The embedded scripting language can be used to do all kinds of things that might
not be immediately useful when writing interactive fiction. For example, one
might make a story that calculates the fibonacci sequence:

~~~
* init, a = 1, b = 2, num = (a + b)

${a}, ${b}...

[fib]: Reticulate splines

* fib
${num}

[fib, a = b, b = num, num = (a + b)]: Fibonacci!
~~~


## License

Storytime is licensed under Gnu General Public License version 3
