# Setup

The simplest way to work on SkillTree is via a virtual machine. These
instructions should work on Linux or Mac - I'm not sure how on
Windows, but should be possible.

First, install the following three packages, via whatever mechanism you have:

- virtualbox
- vagrant
- ansible

Next, move into the directory of the SkillTree source and run the following command:

    vagrant up

It seems to sometimes fail out the first time after it starts to run
the provisioner, so if you see that error, just run:

    vagrant provision

Provisioning is idempotent, so you can do it over and over and nothing
will change.

After that completes, you will have a running virtual machine with all the needed
libraries and binaries. The only thing left to do is to configure the Makefile to
use Vagrant. Open up `Makefile` and set `VAGRANT=1` at the top.

Now run `make init` to configure the package, run `make dbup` to run
all the database migrations, and run `make run` to build and start the
application. It will start the application on port `8000`, so once it
starts up, visit `localhost:8000` in a web browser to see the
application, and when you edit the source files or templates, it'll
automatically recompile.
