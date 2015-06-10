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

## Language

### Sections and Links

Sections are the backbone of Storytime documents and every piece of fiction must
have at least one of them. One can define a new section by starting the line
with an asterisk and following it with section's name. Section names have to
begin with a letter and can contain alphanumeric characters, hyphen (-), or
forward slash (/).

~~~markdown
* beginning

One in a far away land...

[castle]: Explore the castle
[invest]: Become a fairly successful middle manager at a mid-sized investment bank

* castle

You gaze upon the steep walls of the fortress. Climbing them would certainly be
a hurdle.

* invest

On a whim, you decide to look for lucrative investment opportunities in the
derivatives market.

~~~


## Examples

## License

Storytime is licensed under Gnu General Public License version 3
