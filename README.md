mlh-hack-day-erlang-intro
=========================

__Intro to Erlang | MLH Hack Day 2014__

Follow along:
<a targer="_blank" href="https://docs.google.com/presentation/d/1nqoJ0KL7D-JRR-2y1cxq-Tf-Ah3dQTZK_tU3UB28qy8">Presentation by Alexandra Berke</a>

## Getting started

- Download erlang and the OTP library
- Play within the erl interpreter
```
$ erl
Erlang/OTP 17 [erts-6.0] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.0  (abort with ^G)
1> 1 + 1.
2
```
- Compile files from within the erlang interpreter
```
2> c(demo).
{ok,demo}
```


## About file types

- Code is broken up into units called modules
	- modules are in `.erl` files
	- modules __compile__ to `.beam` files
	- eg, `demo.erl` compiles to `demo.beam`
	- `*.beam` files are compiled code that the BEAM Virtual Machine can execute

- `.hrl` files can be included within other files
	- These files are similar to `.h` files in __C__
















