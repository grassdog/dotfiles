# Apps and their config

# TODO Add dotfile business


dep "brews" do
  met? {
    shell? "brew bundle check -v --global"
  }
  meet {
    log_shell "Running brew bundle", "brew bundle -v --global"
  }
end

dep "brew-apps" do
  def brewfile
    "~/.Brewfile.apps"
  end

  met? {
    shell? "brew bundle check -v --file=#{brewfile}"
  }

  meet {
    log_shell "Running brew bundle", "brew bundle -v --file=#{brewfile}"
  }
end

dep "moom-config.file" do
  target "~/Library/Preferences/com.manytricks.Moom.plist"

  source "#{__FILE__.p.parent}/files/com.manytricks.Moom.plist"
end

dep "proselint.pip"

# TODO Turn this into a brew recipe
dep "aws-vault", :version do
  version.default!("3.2.0")

  met? { in_path? "aws-vault" }
  meet {
    path = "/usr/local/bin/aws-vault"
    shell "wget -q -O #{path} https://github.com/99designs/aws-vault/releases/download/#{version}/aws-vault-Darwin-x86_64 && chmod 755 #{path}"
  }
end
