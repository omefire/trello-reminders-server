# trello-reminders-server

A server that exposes a Web API to be used by the [trello-reminders-extension](http://github.com/omefire/trello-reminders-extension).


## How to install & run the server?

0- ```
     * openssl genrsa -out key.pem 2048
     * openssl req -new -key key.pem -out certificate.csr
     * openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
```

1- ```stack build```

2- ```$(stack exec trello-reminders-server-exe)```

3- Access the server by going to different endpoints: http://localhost:8081/getEmailsForUser/omefire@gmail.com
