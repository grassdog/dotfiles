
dep "dotfiles-installed" do
  requires "dotfiles-symlinked",
           "services-copied",
           "editors-installed",
           "tmux-plugin-manager.repo",
           "nvim-config.symlink"
end

#
# Dotfiles
#

dep "dotfiles.repo" do
  source "https://github.com/grassdog/dotfiles"
  path "~/.dotfiles"
end

# Link files

DOTFILES = "#{ENV['HOME']}/.dotfiles"

dotfiles = (
  Dir["#{DOTFILES}/files/.[^.]*"] +
  Dir["#{DOTFILES}/hosts/#{`hostname`.chomp}".p + ".[^.]*"] +
  Dir["#{ENV['HOME']}/Dropbox/Backups/#{`hostname`.chomp}".p + ".[^.]*"]
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

dep "nvim-config.symlink" do
  source "#{DOTFILES}/config/nvim"
  target "~/.config/nvim"
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
        requires "dotfiles-symlinked"
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
  requires "terminfo",
           "vim-installed",
           "emacs-saves.dir"
end

dep "spacemacs.repo" do
  requires "dotfiles-symlinked"

  source "https://github.com/syl20bnr/spacemacs"
  path "~/.emacs.d"
end

dep "terminfo" do
  met? { "~/.terminfo".p.dir? }
  meet {
    shell "tic -o ~/.terminfo /usr/local/Cellar/emacs-plus/24.5/share/emacs/24.5/etc/e/eterm-color.ti"
  }
end

dep "emacs-saves.dir" do
  path "~/.cache/emacs/saves"
end

#
# Vim
#

dep "vim-installed" do
  requires "dotfiles-symlinked",
           "vimrc.symlink",
           "vim-undo.dir",
           "vim-backups.dir",
           "vimplug"
end

dep "vimrc.symlink" do
  requires "dotfiles-symlinked"

  source "~/.dotfiles/files/.vim/vimrc"
  target "~/.vimrc"
end

dep "vim-undo.dir" do
  path "~/.cache/vim/tmp/undo"
end

dep "vim-backups.dir" do
  path "~/.cache/vim/tmp/backups"
end

dep "vimplug" do
  met? {
    "~/.vim/autoload/plug.vim".p.exists?
  }

  meet {
    shell "curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  }
end


#
# tmux
#

dep "tmux-plugins.dir" do
  requires "dotfiles-symlinked"

  path "~/.tmux/plugins"
end

dep "tmux-plugin-manager.repo" do
  requires "tmux-plugins.dir"

  source "https://github.com/tmux-plugins/tpm"
  path "~/.tmux/plugins/tpm"
end

