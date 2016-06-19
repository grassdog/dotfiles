
dep "dotfiles-installed" do
  requires "dotfiles-symlinked",
           "services-copied",
           "editors-installed"
end

#
# Dotfiles
#

dep "dotfiles.repo" do
  source "https://github.com/grassdog/dotfiles"
  path "~/.dotfiles"
end

# Link files

# TODO Fix this path
DOTFILES = "#{ENV['HOME']}/.dotfiles"

dotfiles = (
  Dir["#{DOTFILES}/files/.[^.]*"] +
  Dir["#{DOTFILES}/hosts/#{`hostname`.chomp}".p + ".[^.]*"]
).reject { |r| r =~ /.DS_Store$/ }
 .map(&:p)

dotfile_deps = dotfiles.map { |f|
  base = f.basename.to_s
  name = "#{base}.symlink"

  [
    name,
    -> {
      dep name, template: "symlink" do
        source f.to_s
        target "~/#{base}"
      end
    }
  ]
}

dotfile_deps.each { |name, dep| dep.call }

dep "dotfiles-symlinked" do
  requires "dotfiles.repo"

  dotfile_deps.each { |name, _| requires name }
end

#
# Services
#

dep "services.dir" do
  path "~/Library/Services"
end

services = Dir["#{DOTFILES}/services/*"]
             .map(&:p)

service_deps = services.map { |f|
  base = f.basename.to_s
  name = "#{base}.copieddir"

  [
    name,
    -> {
      dep name, template: "copieddir" do
        source f.to_s
        target "~/Library/Services"
      end
    }
  ]
}

service_deps.each { |name, dep| dep.call }

dep "services-copied" do
  requires "services.dir"

  service_deps.each { |name, _| requires name }
end

#
# Editors
#

dep "editors-installed" do
  requires "spacemacs.repo",
           "vim-installed"
end

dep "spacemacs.repo" do
  source "https://github.com/syl20bnr/spacemacs"
  path "~/.emacs.d"
end

#
# Vim
#

dep "vim-installed" do
  requires "dotfiles-symlinked",
           "vimrc.symlink",
           "vim-undo.dir",
           "vim-backups.dir",
           "vundle.repo"
end

dep "vimrc.symlink" do
  source "~/.dotfiles/files/.vim/vimrc"
  target "~/.vimrc"
end

dep "vim-undo.dir" do
  path "~/.cache/vim/tmp/undo"
end

dep "vim-backups.dir" do
  path "~/.cache/vim/tmp/backups"
end

dep "vundle.repo" do
  source "https://github.com/VundleVim/Vundle.vim"
  path "~/.dotfiles/files/.vim/bundle/Vundle.vim"
end

dep "vundle-install" do
  met? {
    shell "vim +PluginInstall +qall"
  }
end

