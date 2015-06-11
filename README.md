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
begin with a letter and can contain alphanumeric characters, hyphen (-) and
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

<!-- Say something about content and formatting -->

Links connect the sections together and make the story come to live. Links have
to be listed after the content of a section. In the most basic form, a link
consists of a target and a title.

### Variables and Conditions

It is sometimes useful to have conditional links between sections. Conditional
link is only visible if a specified condition is met. Conditions are simple
boolean expressions. Here are a few examples of valid conditions:
`numberOfDragons > 900`, `a = b && b > 10`, `~(a = 1) || (a = 1)`. One can
add a condition to a link by following the target with a bar (|) and the
condition. For example:

~~~

* example

[another | a > 5]: This link is only visible if 'a' is greater than five.
[another | ~(a > 5) ]: This link is only visible if 'a' is not greater than five.

* another

Hurrah!

~~~

Links can also modify variables. The modifications only take place if the link
is followed. If the player can use the link multiple times (for example, if the
sections form loops), the modifications are executed every time. One can add
multiple actions to a single link by separating them with a comma, the actions
will be executed from left to right. 

~~~
* engaging-section

Loop!

[engaging-section | a < 9, +a]: This link leads to the same section and increments 'a' by one every time it is followed. The link will disappear after 'a' becomes greater than ten.
[end| a > 8]: Forward

* end

...that was quite enough

~~~

Actions can contain simple integer expressions. For example, the following are
all valid actions `a = a + 10`, `+power`, `-enemies`, `price = (a * b + c) - d`

Here's another example:

~~~
* first

Some text...

[second, rubber-chicken = 1, points = points + 50]: Take the rubber chicken

* second

~~~

### Embedding and Conditional Content

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
not be immediately useful for writing interactive fiction. For example, one
might make a story that calculates the Fibonacci sequence:

~~~
* init, a = 1, b = 2, num = (a + b)

${a}, ${b}...

[fib]: Reticulate splines

* fib

${num}

[fib, a = b, b = num, num = (a + b)]: Fibonacci!
~~~


## License

Storytime is distributed under the GNU General Public License version 3
