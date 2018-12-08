:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_tail4([_|TL],TL).
my_len5(A,B):-length(A,B).
my_flatten6(A,B):-flatten(A,B).
my_reverse7(A,B):-reverse(A,B).
my_even8(A):-0 is A mod 2.
my_lowercase9(A):-downcase_atom(A,A).
my_double10(N,M):-M is 2*N,M =< 10.
my_sumlist11(A,B):-sumlist(A,B).
my_max_list12(A,B):-max_list(A,B).
my_msort13(A,B):-msort(A,B).
my_set14(A):-list_to_set(A,A).
my_min_list15(A,B):-min_list(A,B).
my_element16(A,B):-member(B,A).
my_succ17(A,B):-succ(A,B),B =< 10.
my_odd18(A):-1 is A mod 2.
my_list_to_set19(A,B):-list_to_set(A,B).
my_head20([H|_],H).
my_last21(A,B):-last(A,B).
my_pred22(A,B):-succ(B,A),A > 0.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_tail4,[list(T),list(T)]).
prim(my_len5,[list(_),int]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_even8,[int]).
prim(my_lowercase9,[char]).
prim(my_double10,[int,int]).
prim(my_sumlist11,[list(int),int]).
prim(my_max_list12,[list(int),int]).
prim(my_msort13,[list(int),list(int)]).
prim(my_set14,[list(_)]).
prim(my_min_list15,[list(int),int]).
prim(my_element16,[list(T),T]).
prim(my_succ17,[int,int]).
prim(my_odd18,[int]).
prim(my_list_to_set19,[list(T),list(T)]).
prim(my_head20,[list(T),T]).
prim(my_last21,[list(T),T]).
prim(my_pred22,[int,int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['H',r,'H',l],[h,h]).
p([j,o,'O',j,'G'],[o,g]).
p(['H',y,f,'Q',j],[h,q]).
p(['H','P',d,l,u,j],[h,p]).
p([q,'E','F','L','Z',c,v,'K'],[e,f,l,z,k]).
q(['E',a,f,o,z],['W',e]).
q(['G','J','K',j],[z,j,k,g]).
q(['T',e,o,i,x,'J',g,o],[j,c,t]).
q(['M','G','E','V'],[g,m,'G',v,e]).
q(['U',a,'V','O'],['Q',v,u,o]).
