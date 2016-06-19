#
# Ruby via chruby
#

dep "global-ruby", :version do
  requires "ruby-installation".with(version: version),
           "global-ruby-version-file".with(version: version)
end

# Build chain

dep "ruby-management" do
  requires "ruby-build.managed",
           "chruby.managed",
           "rubies.dir"
end

RUBIES_DIR = "~/.rubies"

dep "ruby-build.managed"
dep "chruby.managed" do
  met? { "/usr/local/opt/chruby/share/chruby/auto.sh".p.exists? }
end
dep "rubies.dir" do
  path RUBIES_DIR
end

dep "ruby-build up to date" do
  requires "ruby-build.managed"

  before {
    Babushka.host.pkg_helper.update_pkg_lists
  }

  meet {
    shell("brew upgrade ruby-build")
  }
end


# Version file

dep "global-ruby-version-file", :version do
  def file
    "~/.ruby-version"
  end

  def contents
    "#{version}\n"
  end

  met? { file.p.read == contents }
  meet { file.p.write contents }
end

# Ruby

dep "ruby-installation", :version do
  version.ask("Which version of Ruby would you like to install?")

  requires "ruby".with(version: version),
           "gems".with(version: version)
end


dep "ruby", :version do
  requires "ruby-management",
           "ruby-build up to date"

  def build_path
    RUBIES_DIR / version
  end

  met? {
    "#{build_path}/bin/ruby".p.exists?
  }

  meet {
    shell "mkdir -p #{RUBIES_DIR}"
    log_shell "Building via ruby-build", "/usr/local/bin/ruby-build #{version} #{build_path}"
  }
end

# Gems

# Base gems in each ruby version install
GEMS = %w[
  bundler
  pry
  pry-byebug
  pry-stack_explorer
  seeing_is_believing
]

GEMS.each do |gem|
  dep "#{gem}-gem", :version do
    "gem".with(gem_name: gem, ruby_version: version)
  end
end

dep "gems", :version do
  requires "ruby".with(version: version)
  requires GEMS.map {|gem| "#{gem}-gem".with(version: version) }
end

dep "gem", :gem_name, :version, :ruby_version do
  version.default!(:unset)

  requires "chruby.managed"

  def version?
    version != :unset
  end

  def version_switch
    if version?
      "-v #{version}"
    else
      ""
    end
  end

  def version_test
    if version?
      /#{gem_name}\s+\([^(]*#{Regexp.escape version}/
    else
      /#{gem_name}/
    end
  end

  met? {
    log "Checking for gem #{gem_name} #{version_switch} under #{ruby_version}"
    shell("chruby-exec #{ruby_version} -- gem list #{gem_name}") =~ version_test
  }
  meet {
    log "Installing gem #{gem_name} #{version_switch} under #{ruby_version}"
    log_shell "gem install #{gem_name} #{version_switch}", "chruby-exec #{ruby_version} -- gem install #{gem_name} #{version_switch}"
  }
end

