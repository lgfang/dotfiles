# dotfiles

This repository hosts my configuration files.

I intentionally maintain a branch for each application (and also branches for
incompatible versions of configure file). This way, one can pick relevant
versions of configure files in seconds. This is how:

1. Clone this repository into `~/.dotfiles`.

        cd
        git clone https://github.com/lgfang/dotfiles.git .dotfiles
     
2. Create an branch on tag `base` as a start point.

        git branch mine base
        git checkout mine

3. Pick configure files you want.

        git merge origin/bash origin/dir_colors origin/tmux1.8 ...

4. Make adaption per your own needs and commit.

   For example, change user name/mail etc. in the `.gitconfig`

5. Symbolic link configure files to where it should be (the home directory in most cases). Say:

        cd
        ln -s .dotfiles/.bash_profile .
        ln -s .dotfiles/.tmux.conf .
        ln -s .dotfiles/.gitconfig .

6. [Optional] If you'd like ensure that you'll never publish (push) your own branch (which is likely to contain sensitive, confidential information), follow the instructions in `pre-push`.

