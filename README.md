## Example ##

This example shows how to use Websockets and Erlang Web server. All messages
exchanged between client/server are encoded in *binary* format (end-to-end) using
Erlang External Term serialization.

### Getting ###

    $ git clone https://github.com/saleyn/erws_example.git

### Building ###

Initial build:

    $ make deps

Successive build:

    $ make

### Running ###

    $ make run

Open a Web browser (tested with Chrome and Firefox) and point
to your server's address, port 40000.

### License ###

Copyright (c) 2013 Serge Aleynikov

See attached LICENSE file
