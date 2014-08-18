# Setup

The simplest way to work on SkillTree is via a virtual machine. These
instructions should work on Linux or Mac - I'm not sure how on
Windows, but should be possible.

First, install the following three packages, via whatever mechanism you have:

- virtualbox (tested on 4.3.12)
- vagrant (tested on 1.6.3)
- ansible (tested on 1.6.6)
- sshpass (for production provisioning only)

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


# Production

To set up a production deployment, first set the IP address of the
server (should be debian 7.5) in both `provisioning/inventory` and the
`Makefile`. You should already have the VM set up as above (we use the
VM to build production binaries. Not using the VM is not supported if
you want to deploy). First create the production config files. The production
database password is in the ansible vault. So first get the password.txt file
by some means, put it in the provisioning folder, and open up the vault with

    ansible-vault edit --vault-password-file=provisioning/password.txt provisioning/secrets.yml

Take the database password and use it to create the prod.cfg file in
snaplets/persist and snaplets/postgresql-simple, changing the database
name to `skilltree_prod` as well.

If this is a new production server, run `make production-init`. It'll
prompt you for the root password on the server (a few times). If the server
has already been set up, this step isn't needed.

Run database migrations with `make dbup PRODUCTION=1`, and deploy the
application with `make deploy`. Whenever you want to deploy a new
version, just run `make deploy`. If there is an error on startup, the
new version will not be switched to, and you can debug it by looking
at either the application logs (run `make production-log`) or the
logs for the managing process (run `make production-keter-log`).
