GPIOerl
=======

Simple pure erlang module to read and write GPIO connected devices. This
modules allow to read and write GPIO pins via the gpio module and to read
1wire temperature sensors via the w1term module.

Both were tested on the RPI.
Run

    make docs

to build the documentation.

To use the 1wire functionality you need to have these kernel modules loaded:

    w1-gpio
    w1-therm

