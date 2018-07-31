alias mux="mux-identity"

PROMPT_ORDER=(
  time
  status
  context
  git
  ruby
  node
  aws_session
  # elixir
)

source ~/src/identity-dev-bootstrap/files/shell-env.sh

alias aws-actuals="~/src/identity-infrastructure/aws-analysis/rollup-aws-actuals ~/Downloads/ecsv_*.csv | tr '|' ,"

source /usr/local/opt/asdf/asdf.sh
