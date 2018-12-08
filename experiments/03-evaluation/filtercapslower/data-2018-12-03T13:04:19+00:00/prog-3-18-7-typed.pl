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

my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_msort7(A,B):-msort(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_flatten10(A,B):-flatten(A,B).
my_last11(A,B):-last(A,B).
my_even12(A):-0 is A mod 2.
my_set13(A):-list_to_set(A,A).
my_toupper14(A,B):-upcase_atom(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_element18(A,B):-member(B,A).
my_head19([H|_],H).
my_tail20([_|TL],TL).
my_double21(N,M):-M is 2*N,M =< 10.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_sumlist4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_len6,[list(_),int]).
prim(my_msort7,[list(int),list(int)]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_lowercase9,[char]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_last11,[list(T),T]).
prim(my_even12,[int]).
prim(my_set13,[list(_)]).
prim(my_toupper14,[char,char]).
prim(my_succ15,[int,int]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_max_list17,[list(int),int]).
prim(my_element18,[list(T),T]).
prim(my_head19,[list(T),T]).
prim(my_tail20,[list(T),list(T)]).
prim(my_double21,[int,int]).
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
p(['X',a,c,r],[x]).
p([e,o,'T',x,'W',f,o,'R',z],[t,w,r]).
p(['G',h,z,u,'G','G'],[g,g,g]).
p(['X','J',w,k,'L',y,'U',n],[x,j,l,u]).
p(['D','H','M',z,'B',u,'S'],[d,h,m,b,s]).
q([t,a,f,q],['V']).
q(['B','Y','M','B','W','E'],[b,y,b,e,i,m,w]).
q(['L','O','J','X',r],[l,x,o,j,f]).
q(['S',k,'Y','M','K','S','I'],[y,m,s,s,r,k,i]).
q([b,'S',e,'R','E',k,q],[e,s,r,t]).
