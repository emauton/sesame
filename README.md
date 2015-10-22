# sesame
Simple ESP Security Association Manager.

This is a work in progress / proof of concept for a simple system intended to
manage kernel [security
associations](https://en.wikipedia.org/wiki/Security_association) for [IPsec in
transport mode](https://en.wikipedia.org/wiki/IPsec#Transport_mode) running
across many machines.

The idea is to replace most of the
[ISAKMP](https://en.wikipedia.org/wiki/Internet_Security_Association_and_Key_Management_Protocol)
work typically handled by [racoon](http://ipsec-tools.sourceforge.net/) (which
I have found difficult to work with and unnecessarily complex in the transport
context) with a bare-bones session key negotiation over TLS.

## Capabilities
Rather than either running the Erlang VM as root, or making `pfkeyport` setuid
in order to bind a [PF\_KEY](http://tools.ietf.org/html/rfc2367) socket, we set
the [CAP\_NET\_ADMIN capability](http://linux.die.net/man/7/capabilities) on
it, e.g.
    $ make capability
