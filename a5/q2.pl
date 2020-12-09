range(N, N, [N]).
range(M, N, [M|Ns]) :-
  M < N, 
  M1 is M+1, 
  range(M1, N, Ns).

graph_vertices([(U, V)], [U, V]).
graph_vertices([G|Gs], [U,V| Xs]) :-
    G = (U, V),
    graph_vertices(Gs, Xs).

remove(_, [], []).
remove(X, [X|T], Z) :- remove(X, T, Z).
remove(X, [Y|T], [Y|Z]) :- Y \= X, remove(X, T, Z). 

unique_list([], L2, L2).
unique_list([L|L1], L3, L2) :-
    remove(L, L1, L4),
    unique_list(L4, [L|L3], L2).

graph_unique(Gs, Xs) :-
    graph_vertices(Gs, Y),
    unique_list(Y, [], Xs).

remove_edge(_, [], []).
remove_edge(V, [E|Gs], Z) :-
    (E = (V, _); E = (_, V)),
    remove_edge(V, Gs, Z).
remove_edge(V, [E|Gs], [E|Zs]) :-
    E = (U, W),
    not(U = V),
    not(W = V),
    remove_edge(V, Gs, Zs).

vertex_cover(Graph, Cover) :-
    graph_unique(Graph, Vertices),
    vertex_cover(Graph, Vertices, [], Cover).
vertex_cover(UncoveredGraph, Vertices, Vs, Cover) :-
    select(V, Vertices, NewVs),
    remove_edge(V, UncoveredGraph, UncoveredGraph1),
    vertex_cover(UncoveredGraph1, NewVs, [V|Vs], Cover).
vertex_cover([], _, Cover, Cover).


/*[(1, 3), (2, 3), (3, 4), (4, 5), (4, 6), (5, 6)]*/