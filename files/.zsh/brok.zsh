export PATH="~/.composer/vendor/bin:/usr/local/opt/postgresql@9.6/bin:${PATH}"

PROMPT_ORDER=(
  time
  status
  context
  git
  ruby
  node
  aws_session
  elixir
)

###################
# Version managers
###################

eval "$(nodenv init -)"

source /usr/local/opt/asdf/asdf.sh

# Chruby
if [[ -e /usr/local/opt/chruby/share/chruby/chruby.sh ]]; then
  source /usr/local/opt/chruby/share/chruby/chruby.sh
  source /usr/local/opt/chruby/share/chruby/auto.sh
  [[ -r ~/.ruby-version ]] && chruby $(cat ~/.ruby-version)
  [[ -r ./.ruby-version ]] && chruby $(cat ./.ruby-version)
fi

# Look for trusted local binaries in path
export PATH="./.git/safe/../../bin:${PATH}"

##############
# AWS Helpers
##############

# Execute a command
alias awscp="aws-vault exec customer-production --assume-role-ttl=1h --"
alias awscd="aws-vault exec customer-development --assume-role-ttl=1h --"

# Log into the web console
alias awscp-login="aws-vault login customer-production"
alias awscd-login="aws-vault login customer-development"

# Fire up a shell
awscp-shell() {
  awscp env AWS_SESSION_NAME='cust-prod' $SHELL
}

awscd-shell() {
  awscd env AWS_SESSION_NAME='cust-dev' $SHELL
}

cleanoutcreds() {
  unset AWS_SESSION_TOKEN
  unset AWS_SECRET_ACCESS_KEY
  unset AWS_ACCESS_KEY_ID
  unset AWS_DEFAULT_REGION
}


######
# SSH
######

alias ssh-prod="ssh identity_production_bastion"
alias ssh-staging="ssh identity_staging_bastion"

ssh-add-all() {
  local keys=(
    ~/.ssh/id_rsa
    ~/.ssh/aws/identity-production-20180124-us-east-1.pem
    ~/.ssh/aws/identity-staging-2-ap-southeast-2.pem
    ~/.ssh/aws/identity-production-1-us-west-1.pem
  )

  for key in "${keys[@]}"; do
    [[ -r $key ]] && ssh-add -K $key
  done
}

alias aws-actuals="~/src/identity-infrastructure/aws-analysis/rollup-aws-actuals ~/Downloads/ecsv_*.csv | tr '|' ,"

