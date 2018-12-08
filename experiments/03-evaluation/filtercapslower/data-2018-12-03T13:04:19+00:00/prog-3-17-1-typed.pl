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
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_reverse7(A,B):-reverse(A,B).
my_head8([H|_],H).
my_lowercase9(A):-downcase_atom(A,A).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B),B =< 10.
my_len12(A,B):-length(A,B).
my_element13(A,B):-member(B,A).
my_flatten14(A,B):-flatten(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_toupper16(A,B):-upcase_atom(A,B).
my_min_list17(A,B):-min_list(A,B).
my_pred18(A,B):-succ(B,A),A > 0.
my_last19(A,B):-last(A,B).
my_even20(A):-0 is A mod 2.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_tail4,[list(T),list(T)]).
prim(my_odd5,[int]).
prim(my_set6,[list(_)]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_head8,[list(T),T]).
prim(my_lowercase9,[char]).
prim(my_max_list10,[list(int),int]).
prim(my_succ11,[int,int]).
prim(my_len12,[list(_),int]).
prim(my_element13,[list(T),T]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_sumlist15,[list(int),int]).
prim(my_toupper16,[char,char]).
prim(my_min_list17,[list(int),int]).
prim(my_pred18,[int,int]).
prim(my_last19,[list(T),T]).
prim(my_even20,[int]).
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
p(['Q',f,'J',r,'W','U'],[q,j,w,u]).
p([b,i,y,g],[]).
p([c,'C',f,f,'A','I','J'],[c,a,i,j]).
p([v,q,y,u,'E',w,f,'O'],[e,o]).
p(['G','O','C','J','O','Q','C'],[g,o,c,j,o,q,c]).
q([o,a,'L','X','O',u,'Y'],[o,y,l,'Q',x]).
q([g,h,'N','K','W','V',o],[w,k,n,v,'R']).
q(['T','E','R','J','M','Z'],[m,e,'Z',z,t,j,r]).
q(['B',j,'B','Y','E',v,c],[e,b,b,u,y]).
q(['N','I',k,l,'N','F',m,b],[n,i,n,s,f]).
