# This should be here but for some reason when it is the PATH isn't set correctly
# source /opt/boxen/env.sh

[[ -r "$HOME/.secrets" ]] && source "$HOME/.secrets"

##############
# AWS Helpers
##############

alias awsir="aws-vault exec identity-production-readonly -t 60m --"
alias awsip="aws-vault exec identity-production-admin -t 60m --"
alias awsid="aws-vault exec identity-development -t 60m --"

for profile in envatomarket-prod identity-production-readonly identity-production identity-development; do
  alias aws-console-${profile}="open \$(aws-vault login ${profile})"
  alias aws-exec-${profile}="aws-vault exec ${profile} -- true && aws-vault exec ${profile} -- "
  alias aws-login-${profile}="eval \$(aws-exec-${profile} env | grep AWS | sed 's/^/export /g')"
  alias aws-clear-${profile}="security delete-generic-password -a '${profile}'"
done

cleanoutcreds() {
  unset AWS_SESSION_TOKEN
  unset AWS_SECRET_ACCESS_KEY
  unset AWS_ACCESS_KEY_ID
  unset AWS_DEFAULT_REGION
}

awsir-shell() {
  awsip env AWS_SESSION_NAME='id-prod-ro' $SHELL
}

awsip-shell() {
  awsip env AWS_SESSION_NAME='id-prod' $SHELL
}

awsid-shell() {
  awsid env AWS_SESSION_NAME='id-dev' $SHELL
}

######
# SSH
######

alias ssh-prod="ssh bastion.identity.us-east-1"
alias ssh-staging="ssh bastion.identity.ap-southeast-2"
