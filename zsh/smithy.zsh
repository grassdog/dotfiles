# This should be here but for some reason when it is the PATH isn't set correctly
# source /opt/boxen/env.sh

#eval $(docker-machine env default)

alias awsi="aws-vault exec identity-production-admin --"

export GOPATH=$HOME/.golang
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin
