% Main file to run the pengines demo.
% Starts server on port 3030 and keeps the process alive.

:- use_module(library(settings)).
:- use_module(library(pengines)).
:- use_module(library(http/http_cors)).

% enable CORS for all domains
:- set_setting(http:cors, [*]).

% load project-specific code
:- [load].

% Entry point that blocks to keep the container alive
:- initialization(main, main).

main :-
    server(3030),
    format('% Started server at http://localhost:3030/~n'),
    % Block forever (wait for a message that never arrives)
    thread_get_message(_Stop).
