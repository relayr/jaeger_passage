Files in this directory have been automatically generated from [jaeger-idl](https://github.com/jaegertracing/jaeger-idl/blob/b59e84ebb35cd858614f836e47540d6f95e0cf88/thrift) files:
* agent.thrift _(commented out zipkin related lines here)_
* jaeger.thrift

Command used:
```
thrift -r --gen erl:maps -out src/thrift agent.thrift
```
