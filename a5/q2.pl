range(N, N, [N]).
range(M, N, [M|Ns]) :-
  M < N, 
  M1 is M+1, 
  range(M1, N, Ns).

graph_vertices([(U, V)], [U, V]).
graph_vertices([G|Gs], [U,V| Xs]) :-
    G = (U, V),
    graph_vertices(Gs, Xs).

graph_unique(Gs, Xs) :-
    graph_vertices(Gs, Y),
    max_list(Y, Max),
    min_list(Y, Min),
    range(Min, Max, Xs).

remove_edge(_, [], []).
remove_edge(V, [E|Gs], Z) :-
    (E = (V, _); E = (_, V)),
    remove_edge(V, Gs, Z).
remove_edge(V, [E|Gs], [E|Zs]) :-
    E = (U, W),
    not(U = V),
    not(W = V),
    remove_edge(V, Gs, Zs).

in_graph(V, [E|_]) :-
    E = (V, _);
    E = (_, V).
in_graph(V, [E|Gs]) :-
    E = (U, W),
    not(U = V),
    not(W = V),
    in_graph(V, Gs).

vertex_cover(Graph, Cover) :-
    graph_unique(Graph, Vertices),
    vertex_cover(Graph, Vertices, [], Cover).
vertex_cover(UncoveredGraph, Vertices, Vs, Cover) :-
    select(V, Vertices, NewVs),
    remove_edge(V, UncoveredGraph, UncoveredGraph1),
    vertex_cover(UncoveredGraph1, NewVs, [V|Vs], Cover).
vertex_cover([], _, Cover, Cover).

/*
vertex_cover([], _).
vertex_cover(Graph, [V|Cover]) :-
    in_graph(V, Graph),
    remove_edge(V, Graph, UncoveredGraph),
    vertex_cover(UncoveredGraph, Cover).
*/

/*[(1, 3), (2, 3), (3, 4), (4, 5), (4, 6), (5, 6)]*/