FROM mcr.microsoft.com/devcontainers/base:jammy

USER vscode
WORKDIR /home/vscode

RUN curl https://ciao-lang.org/boot -sSfL | sh -s -- --prebuilt-bin local-install