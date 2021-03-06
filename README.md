### Running tests
```erlang
rebar3 eunit --dir "apps/core/tests"
```

### Running server in console
```erlang
rebar3 shell --apps library
```

### Running named node 
```erlang
rebar3 shell --sname <name> --apps library
```

### Connecting to node
```erlang
net_kernel:connect_node('servername@hostname').
```

### Use case diagram

![alt text](docs/Library_use_case_diagram.jpg)

### Architecture

![alt text](docs/Library_architecture.jpg)