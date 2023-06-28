[![Docker Image CI](https://github.com/Ant2000s/webRTP/actions/workflows/docker-image.yml/badge.svg)](https://github.com/Ant2000s/webRTP/actions/workflows/docker-image.yml)

webRTP
=====

WebRTP is application for sending voice messages
to abonents using REST API. The application uses
a database to store abonents data. The user can
also get/add/remove them using HTTP requests.

Getting Started
-----
The application runs in a docker container, so
you need to install Docker. Then run the following
commands:
```
git clone https://github.com/Ant2000s/webRTP.git
cd webRTP
docker-compose -f release.yaml -p webrtp up
```
The project can also be built and run without using
docker-compose:
```
docker build -t webrtp .
docker run -it -p 8080:8080 webrtp
```
Features
-----
All requests are sent to localhost:8080/

| Method | Request                    | Description                                                                                                            |
|--------|----------------------------|------------------------------------------------------------------------------------------------------------------------|
| GET    | abonent/ID/                | Getting abonent data by ID, where ID is an integer value.                                                              |
| GET    | abonents/                  | Getting a list of IDs of all abonents in the database.                                                                 |
| GET    | call/abonent/ID/"MESSAGE"/ | Calling abonent with the specified ID and sending him a voice message with the text MESSAGE.                           |
| GET    | call/broadcast/"MESSAGE"/  | Сalling and sending the specified message to all abonents in the database.                                             |
| POST   | abonent/                   | Adding abonent to the database. The request body must contain ID and Pass fields. Example: {"ID": 102, "Pass": "1234"} |
| DELETE | abonent/ID/                | Deleting abonent with the specified ID.                                                                                |
Important
-----
Since the application was written for a specific server, the application itself is registered as abonent 101.
Initially, abonents 102, 104 and 105 are present in the database.