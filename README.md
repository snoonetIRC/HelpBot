# Snoonet HelpBot

This bot sends ctcp requests to those that join a channel and sends the results to people who have defined modes within the channel.

## Installation

This bot requires stack to run, to install stack on linux run `curl -sSL https://get.haskellstack.org/ | sh` or visit [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) for more information

Once stack is installed, you need run the following commands to successfully build HelpBot:

1. `stack init`
2. `stack setup`
3. `stack build`

Once HelpBot is built, you can run it with `stack exec helpbot-exe`
