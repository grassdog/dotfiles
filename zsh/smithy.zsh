# This should be here but for some reason when it is the PATH isn't set correctly
# source /opt/boxen/env.sh

#eval $(docker-machine env default)

alias awsip="aws-vault exec identity-production-admin --"
alias awsid="aws-vault exec identity-development --"

ssh-id-prod() {
  pushd ~/src/identity/infrastructure
  awsip bundle exec rake production:ssh
}

export GOPATH=$HOME/.golang
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

pj() {
  cd $(find ~/src -maxdepth 1 -type d | selecta)
}

# Some AWS Helpers

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
