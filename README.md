openage matchmaking and lobbies
===============================

What's this?
------------

This is the "master server" for [openage](http://openage.sft.mx).

Features:

* [ ] Dedicated server registry for public lobbies
* [ ] Player accounts
* [ ] Match results signed with [GPG](https://www.gnupg.org/)
* [ ] Player and match statistics
* [ ] [Elo](https://en.wikipedia.org/wiki/Elo_rating_system) rankings and match making


When you want to play **with friends** via LAN, VPN or Internet,
this server is not required for you.

This server provides available public lobbies and can generate
balanced battles through Elo matchmaking.


How do I run this thing?
------------------------

You probably don't want to run it except for development:
To have one "official" community, this server is provided by sft.

The openage masterserver uses the haskell tool stack to build and
install.
Executables can be built using `stack build`.

The server can be started by running `stack exec openage-masterserver`.
The port it is listening on and the database login credentials are
specified in the config file /etc/openage/masterserver.cfg.

To start a testclient use
`stack exec openage-masterserver-test HOST PORT`
You will be promted to enter login credentials which need to be stored
in the postgres database.
To view all available commands type `help`.


Can I help?
-----------

Yay! You can just start hacking on whatever you like to improve.
Fix bugs, implement features, submit pull requests!

If you got any question, join our IRC: `#sfttech` on `irc.freenode.net`.


License
-------

**GNU AGPLv3** or later; see [copying.md](copying.md) and [legal/AGPLv3](legal/AGPLv3).

I know that probably nobody is ever gonna look at the `copying.md` file,
but if you want to contribute code to openage, please take the time to
skim through it and add yourself to the authors list.
