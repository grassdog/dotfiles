# This should be here but for some reason when it is the PATH isn't set correctly
# source /opt/boxen/env.sh

[[ -r "$HOME/.secrets" ]] && source "$HOME/.secrets"

#eval $(docker-machine env default)

ssh-id-prod() {
  pushd ~/src/identity/infrastructure
  awsip bundle exec rake production:ssh
}

# export GOPATH=$HOME/.golang
# export GOROOT=/usr/local/opt/go/libexec
# export PATH=$PATH:$GOPATH/bin
# export PATH=$PATH:$GOROOT/bin

pj() {
  cd $(find ~/src -maxdepth 1 -type d | selecta)
}

# AWS Helpers

alias awsir="aws-vault exec identity-production-readonly -t 60m --"
alias awsip="aws-vault exec identity-production -t 60m --"
alias awsipa="aws-vault exec identity-production-admin -t 60m --"
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

assumerole() {
  cleanoutcreds
  eval $(aws $1 sts assume-role --role-arn $2 --role-session-name assume-role --serial-number arn:aws:iam::518040037068:mfa/andrew.humphrey --token-code $(otp envato-aws-users false) --query 'Credentials.{Key:SecretAccessKey,Token:SessionToken,Id:AccessKeyId}' | sed -e 's/[{},]*//g' | sed -e 's/"Token": /export AWS_SESSION_TOKEN=/' -e 's/"Id": /export AWS_ACCESS_KEY_ID=/' -e 's/"Key": /export AWS_SECRET_ACCESS_KEY=/')
}

assumerole2() {
  aws sts assume-role \
  --role-arn ${ASSUME_ROLE_ARN} \
  --role-session-name ${SESSION_NAME} \
  --query 'Credentials.{AWS_ACCESS_KEY_ID:AccessKeyId,AWS_SECRET_ACCESS_KEY:SecretAccessKey,AWS_SESSION_TOKEN:SessionToken}' \
  | jq -r '@sh "export AWS_ACCESS_KEY_ID=\(.AWS_ACCESS_KEY_ID)
  export AWS_SECRET_ACCESS_KEY=\(.AWS_SECRET_ACCESS_KEY)
  export AWS_SESSION_TOKEN=\(.AWS_SESSION_TOKEN)"'
}

alias ssh-prod="ssh bastion.identity.us-east-1"
alias ssh-staging="ssh bastion.identity.ap-southeast-2"
