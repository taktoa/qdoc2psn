# qdoc2psn

This program automatically generates [purescript-native][] FFI bindings for
arbitrary C++11 libraries.

It does so by parsing the XML output of the Qt documentation generator, `qdoc`.
This allows it to specially handle many features of Qt-based libraries, while
still working for generic C++ libraries (assuming you're at least willing to
deal with a codegen-time dependency on Qt).

# Current progress

The current state of affairs is that this program can parse the XML output of
`qdoc`, but neither PureScript nor C++11 wrapper codegen has been implemented
yet.

My goal is to get this working to the extent that I can use it fruitfully for
my other project, [arcane-chat][].

Once this project is working, patches are welcome. Until then, contact me on
IRC or via email before you start on anything major (I idle in #purescript on
FreeNode under the same username, and my email is <taktoa@gmail.com>).

# Future work

## GLib/GTK

In the future, I would also consider adding support for the [GIR][] XML format,
which would give special support for GLib/GTK-based libraries like `gtkmm`.

I don't consider this to be quite as much of a priority since as far as I know
there is much more demand for a fast, Haskell-like, cross-compilable language
with good Qt bindings.

I'm mostly interested in this feature because it would streamline creation of
bindings for GStreamer.

--------------------------------------------------------------------------------

[purescript-native]: https://github.com/andyarvanitis/purescript-native
[GIR]:               https://developer.gnome.org/gi/stable/gi-gir-reference.html
[arcane-chat]:       https://github.com/taktoa/arcane-chat
