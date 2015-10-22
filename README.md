# sesame
Simple ESP Security Association Manager.

This is a work in progress / proof of concept for a simple system intended to
manage kernel security associations for IPsec in transport mode running across
many machines.

The idea is to replace most of the ISAKMP work typically handled by `racoon`
(which I have found difficult to work with and unnecessarily complex in the
transport context) with a bare-bones session key negotiation over TLS.
