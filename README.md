# CXX Template

Be aware, altering files from this repository may yield merge conflicts upon
updating. The setup script will replace this README, you are free to change and
commit it afterwards.

Fork this and run the setup utility, an example module will be added:

    $ scripts/setup/run <project-name>

You can use the script to add new, empty modules, just use the same project
name. Example:

    $ scripts/setup/run insieme frontend core backend

Additionally a script is provided to add new *parts* to a module. The target
module must already exist and you have to provide the same project-name.

    $ scripts/setup/add_part insieme frontend sema

Previous command adds a `sema.h`, `sema.cpp` and `sema_test.cc` to the corresponding directories of the *frontend* module. You can also supply multiple *parts*.

    $ scripts/setup/add_part insieme frontend sema extensions/malloc_extension

Subdirectories inside a module are created along the way as needed.

## Template Development

The development of this template continues in its own branch `template` and
will be merged to `master` from time to time. Each merge will mark a new
version of the template.
