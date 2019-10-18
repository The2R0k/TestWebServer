# TestWebServer
Just web servers for tests
Erlang/OTP 20


#
# Quick start (Ubuntu):
#

```
sudo apt install erlang redis
sudo systemctl start redis-server
git clone https://github.com/The2R0k/TestWebServer.git
cd TestWebServer
make chmod-rebar
make build-and-run
```

# Send post request:
 ```
 curl -X POST -d '{
"links": [
"https://ya.ru",
"https://ya.ru?q=123",
"funbox.ru",
"https://stackoverflow.com/questions/11828270/how-to-exit-the-.."
]
}' -v -i 'http://localhost:8080/visited_links'
```

# Send get request:
```
curl -X get -v -i 'http://localhost:8080/visited_domains?from=123&to=99999999999999999999'
```

Web server supported only 2 methods (GET/POST) and 2 commands for method GET (from=/to=)
Commands is not necessary. Default value for 'from=' 0. Default value for 'to=' 10^20-1.

# Tests

you can use the command 'make test' to run the tests for project
