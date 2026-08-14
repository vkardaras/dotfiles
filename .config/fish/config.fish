if status is-interactive
    # Commands to run in interactive sessions can go here
end

starship init fish | source

# Set ANDROID_HOME environment variable
set -x ANDROID_HOME $HOME/Android/Sdk

# Add SDK tools to PATH
fish_add_path $ANDROID_HOME/platform-tools
fish_add_path $ANDROID_HOME/cmdline-tools/latest/bin
fish_add_path $ANDROID_HOME/emulator   