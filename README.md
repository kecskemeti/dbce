# dbce

A simple lichess chess bot written in rust

Before using this program, make sure you register a bot account
on [lichess](https://lichess.org/blog/WvDNticAAMu_mHKP/welcome-lichess-bots). Once registered, you also need to generate
an authorization token for the program to function.

* Compile: cargo build --release
* Run: target/release/dbce

When the program starts it will ask for your auth token and your bot's lichess user id. Once you enter these, you are
all set, the bot is active on lichess. It will give status updates on its performance and activity on the console.
