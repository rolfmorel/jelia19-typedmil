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

my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_tail7([_|TL],TL).
my_flatten8(A,B):-flatten(A,B).
my_len9(A,B):-length(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_element12(A,B):-member(B,A).
my_odd13(A):-1 is A mod 2.
my_lowercase14(A):-downcase_atom(A,A).
my_max_list15(A,B):-max_list(A,B).
my_min_list16(A,B):-min_list(A,B).
my_head17([H|_],H).
my_reverse18(A,B):-reverse(A,B).
my_double19(N,M):-M is 2*N,M =< 10.
my_set20(A):-list_to_set(A,A).
my_pred21(A,B):-succ(B,A),A > 0.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_succ6,[int,int]).
prim(my_tail7,[list(T),list(T)]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_len9,[list(_),int]).
prim(my_toupper10,[char,char]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_element12,[list(T),T]).
prim(my_odd13,[int]).
prim(my_lowercase14,[char]).
prim(my_max_list15,[list(int),int]).
prim(my_min_list16,[list(int),int]).
prim(my_head17,[list(T),T]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_double19,[int,int]).
prim(my_set20,[list(_)]).
prim(my_pred21,[int,int]).
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
p(['G','Y','W',l,'Z'],[g,y,w,z]).
p([n,'O',b,'Z','W'],[o,z,w]).
p(['L','J','N','E','K','W','Q','F'],[l,j,n,e,k,w,q,f]).
p(['Y','W','K',p,'G','H',t],[y,w,k,g,h]).
p(['E',r,'Q',p,'K',b],[e,q,k]).
q(['F','D','J',u,p,'X','H',l],[f,d,x,h,'L',j]).
q([c,'X','W',g,a,l],[x,w,'H']).
q(['R',j,a,'V',e,'R'],[r,v,j,r]).
q(['W',k,'F','B'],[w,b,f,d]).
q([f,'F',y,'F'],[f,r,f]).
