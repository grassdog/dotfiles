#
# Node via nvm
#

module NVMHelpers
  def nvm_path
    @nvm_path ||= shell "echo $(brew --prefix nvm)/nvm.sh"
  end

  def nvm_shell(command)
    shell "NVM_DIR=#{NVM_DIR} source #{nvm_path} && #{command}"
  end
end

dep "default-node", :version do
  requires "node-installation".with(version: version),
           "node-default".with(version: version)
end

# Version management

dep "node-default", :version do
  extend NVMHelpers

  requires "node".with(version)

  met? { nvm_shell("nvm ls").split.grep(/default -> #{version}/) }
  meet {
    nvm_shell "nvm link default #{version}"
  }
end

# Management

NVM_DIR = "~/.nvm"

dep "node-management" do
  requires "nvm.dir", "nvm.managed"
end

dep "nvm.dir" do
  path NVM_DIR
end

dep "nvm.managed" do
  extend NVMHelpers

  met? {
    nvm_path.p.exist?
  }
end

# Node

dep "node-installation", :version do
  version.ask("Which version of Node would you like to install?")

  requires "node".with(version: version),
           "npm-libs".with(version: version)
end

dep "node", :version do
  extend NVMHelpers

  requires "node-management"

  met? {
    nvm_shell("nvm ls").split.grep(/v#{version}/)
  }

  meet {
    nvm_shell "nvm install #{version}"
  }
end

# NPM

LIBS = %w[
  gulp
  js-beautify
  prettier
  coffee-script
  tern
  elm
  elm-oracle
  jsonlint
  grasp
]

LIBS.each do |lib|
  dep "#{lib}.npm"
end

dep "npm-libs", :version do
  requires "node".with(version: version)
  requires LIBS.map {|lib| "#{lib}.npm" }
end

