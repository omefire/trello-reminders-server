# trello-reminders-server

A server that exposes a Web API to be used by the [trello-reminders-extension](http://github.com/omefire/trello-reminders-extension).


## How to install & run the server?

1- ```stack build```

2- ```$(stack exec trello-reminders-server-exe)```

3- Access the server by going to different endpoints: http://localhost:8081/getEmailsForUser/omefire@gmail.com