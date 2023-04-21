# GPTi

GPTi is a simple OpenAI API client that makes use of GPT and transcription models.
It provides a REPL (Read-Eval-Print Loop) interface for typing questions and receiving answers from the GPT model.
GPTi also supports speech input and output, allowing you to speak your questions and hear the synthesized answers, very much like the original StarTrek computer.

## Features

- Type questions and receive answers from OpenAI's GPT models.
- Ask questions with your voice and receive answers as synthesized speech.
- Use commands to quote files, request spoken output, and start recording from your microphone.
- Predefined local functions that the GPT model can call.

## Installation

If you are not particularily experienced with building Haskell programs,
the easiest way to install GPTi is to use Haskell Stack.
You might be able to install `stack` with your distributions package manager.
Alternatively, you can use the following command:

```bash
wget -qO- https://get.haskellstack.org/ | sh
```

To install GPTi, follow these steps:

1. Clone the GPTi repository from GitHub:

   ```bash
   git clone https://github.com/mlang/gpti
   cd gpti
   ```

2. Build and install GPTi using Stack:

   ```bash
   stack install
   ```

## Configuration

### API Key

To use GPTi, you will need an OpenAI API key.
There are two ways to configure the API key:

1. If you are using the `pass` utility to store your passwords encrypted, insert your API key and set the `OPENAI_API_KEY_PASS_NAME` environment variable to the name. GPTi will invoke the `pass show` command to retrieve the key during startup.

   ```bash
   export OPENAI_API_KEY_PASS_NAME=OpenAI/API_Key
   pass insert "$OPENAI_API_KEY_PASS_NAME"
   ```

   You will probably want to set `OPENAI_API_KEY_PASS_NAME` in your `~/.profile`.

2. Alternatively, set the `OPENAI_API_KEY` environment variable to your API key.
   This is less secure as it will make your API key available to all external programs you run in your shell session.

### User Settings

GPTi can be customized via a YAML configuration file.
An example configuration file can be found in [examples/config.yaml](https://github.com/mlang/gpti/blob/master/examples/config.yaml).
By default, GPTi expects the config file to be located at `$XDG_CONFIG_HOME/gpti/config.yaml` which typically expands to `$HOME/.config/gpti/config.yaml`.

#### recorder

This setting allows you to customize the arguments passed to the program used to record speech input.
Currently, only `ffmpeg` is supported.
GPTi will append the necessary arguments to encode the audio and save it to a temporary file.
All you need to provide is the part which specifies where to record from.

#### speaker

Similar to `recorder`, this setting allows to customize the arguments passed to the speech synthesizer.
Currently, only `espeak-ng` is supported.

#### chat

Here you can configure the default chat session parameters, like `temperature` or the `model`.  The `messages` key can be used to provide a default set of messages which should always be started from.

```yaml
  messages:
  - role: system
    content: |
      You are assisting a person capable of installing a Haskell Stack project.

      Please be succinct and remember to use Markdown markup whenever possible.
```

#### chat_functions

GPTi provides the following predefined functions that can be called by the GPT model:

* `localTime`: Get the current local time of the user's machine.
* `metar`: Retrieve METAR weather information for an airport with an ICAO code.
  The model typically already knows ICAO codes for popular airports, so you can ask for the weather in a particular city.
* `haskell`: Evaluate pure Haskell expressions (no IO) provided by the model.
  This allows the model to give more accurate answers when numbers are involved.

This is only an initial set of possible functions.
New functions are easy to write (see [GPTi.Functions](https://github.com/mlang/gpti/blob/master/GPTi/Functions.hs)) and interesting PRs are encouraged.

#### transcription

Here you can provide default parameters for the audio transcription.
