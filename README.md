![GitHub](https://img.shields.io/github/license/kecskemeti/dbce) ![GitHub top language](https://img.shields.io/github/languages/top/kecskemeti/dbce)  ![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/kecskemeti/dbce/rust.yml) ![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/kecskemeti/dbce/test.yml?label=tests)

# dbce

A simple chess bot written in Rust integrated with its own [Lichess](https://lichess.org) API client.

* Compile: `cargo build --release`
* Run the lichess bot: `target/release/lichess`
    * This can also be done by running `cargo run`
    * When the program starts it will ask for your auth token and your bot's lichess user id. Once you enter these, you
      are all set, the bot is active on lichess. It will give status updates on its performance and activity on the
      console.
    * Before using this program, make sure you register a bot account
      on [lichess](https://lichess.org/blog/WvDNticAAMu_mHKP/welcome-lichess-bots). Once registered, you also need to
      generate an authorization token for the program to function.
    * The program will automatically challenge other bots from time to time.
    * While it is playing, it will not challenge, nor will it accept any challenges.
    * It will challenge for all kinds of time controls from ultra bullet to rapid, with and without time increment.
    * The bot will try to adjust its depth of search based on its remaining time.
    * **WARNING**: you will need plenty of memory to run this current version if it goes beyond particular depths. At
      the moment, it needs around 40GiB for a 2.5 move look ahead.
* To experiment with the bot locally, run: `target/release/local`
    * This can also be done by running `cargo run --bin local`
    * Here the machine will play against you on the console. Make sure you run it on a console with unicode character
      support, otherwise you will not see the pieces rendered correctly.
    * You can enter your moves in the
      traditional [chess algebraic notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)).

#### Disclaimer

The development of this project was supported by [JetBrains](https://jb.gg/OpenSourceSupport). 