FROM haskell:8.6.3
RUN mkdir -p /opt/trello-reminders-server
ADD . /opt/trello-reminders-server
ADD ./credentials.json /opt/trello-reminders-server/credentials.json
WORKDIR /opt/trello-reminders-server
RUN apt-get update && apt-get install libpq-dev -y
RUN stack setup
RUN stack build
CMD [".stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/trello-reminders-server-exe/trello-reminders-server-exe"]