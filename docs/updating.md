# Updating the docs

Updating the documentation is easy and should be done as users
discover useful tips and tricks along their own workflows. All
documentation is stored on github in plain-text at
https://github.com/TACC/EnviroTyping.

## Accessing the source


Make a working copy of the documentation from
https://github.com/TACC/EnviroTyping

**from the terminal**

from your working directory, download the project from github::

     git clone https://github.com/TACC/EnviroTyping.git

After a change has been made to the master repository, `readthedocs
<https://readthedocs.org>`_ automatically builds fresh html
documentation hosted on their servers.

**from the desktop**

browse to https://github.com/TACC/EnviroTyping

click "Clone or Download" at the right. You can use the github desktop
app or work with the compressed folder using the text editor of your
choice. For comprehensive edits you may wish to use `Atom
<https://atom.io>`_ with the markdown preview package enabled with the
documentation directory selected as your project folder.


For more on MkDocs / Read the Docs, see:

- http://www.mkdocs.org/#getting-started
- http://docs.readthedocs.io/en/latest/

## Forking & Committing Changes


Follow the standard git commit process to request changes. For a full
introduction see:

- https://help.github.com/articles/fork-a-repo/
- https://yangsu.github.io/pull-request-tutorial/
- http://swcarpentry.github.io/git-novice/

In short, you'll want to create a fork of the repository from the
terminal or the github project's website. The repository includes
markdown source files which previewing in html on your own machine.
Once you've finished with your proposed changes, add & commit the
changes to your fork & open a pull request to the master branch at
TACC/EnviroTyping/docs.


## How it Works


MkDocs is a fast, simple and downright gorgeous static site generator
that's geared towards building project documentation. Documentation
source files are written in Markdown, and configured with a single YAML
configuration file.


Contact hiltbrandd@uncw.edu
