dep "dev-env" do
  requires "zsh-shell",
           "dev-folders",
           "colourpickers",
           "ssh keys generated"
end

#
# Shell
#

dep "zsh-shell", :username do
  username.default!(shell("whoami"))
  requires "zshenv-fixed"
  met? { shell("sudo su - '#{username}' -c 'echo $SHELL'") == which("zsh") }
  meet { sudo("chsh -s '#{which('zsh')}' #{username}") }
end

dep "zshenv-fixed" do
  on :osx do
    met? { !"/etc/zshenv".p.exists? && "/etc/zprofile".p.exists? }
    meet {
      shell "sudo mv /etc/zshenv /etc/zprofile"
    }
  end
end

#
# Folders
#

dep "dev-folders" do
  requires "dev.dir",
           "scripts.symlink",
           "scratch.dir",
           "old.dir",
           "btsync.dir"
end

dep "dev.dir" do
  path "~/dev"
end

dep "scripts.symlink" do
  requires "dev.dir"

  source "~/Dropbox/Code/scripts"
  target "~/dev/scripts"
end

dep "scratch.dir" do
  requires "dev.dir"
  path "~/dev/scratch"
end

dep "btsync.dir" do
  path "~/btsync"
end

dep "old.dir" do
  requires "dev.dir"
  path "~/dev/old"
end

#
# Colour pickers
#

dep "colourpickers" do
  requires "dev colour picker"
end

dep "colorpickers.dir" do
  path "~/Library/ColorPickers"
end

dep "dev colour picker", template: "archive" do
  requires "colorpickers.dir"

  source "http://download.panic.com/picker/developercolorpicker.zip"
  name "DeveloperColorPicker.colorPicker"
  target "~/Library/ColorPickers/DeveloperColorPicker.colorPicker"
end

#
# SSH
#

dep 'ssh keys generated', :ssh_dir, :ssh_password do
  ssh_dir.ask("Where do you keep your ssh keys").default!('~/.ssh')
  ssh_password.ask("Passphase to encrypt your SSH key")

  met? {
    (ssh_dir / 'id_rsa.pub').exists? && (ssh_dir / 'id_rsa').exists?
  }
  meet {
    shell "ssh-keygen -t rsa -N #{ ssh_password } -f #{ ssh_dir }/id_rsa"
  }
  after {
    shell "/usr/bin/ssh-add -K"
  }
end
