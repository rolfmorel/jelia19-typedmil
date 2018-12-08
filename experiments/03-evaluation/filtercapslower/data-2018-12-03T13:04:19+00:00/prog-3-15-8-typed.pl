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
my_reverse5(A,B):-reverse(A,B).
my_last6(A,B):-last(A,B).
my_len7(A,B):-length(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_double10(N,M):-M is 2*N,M =< 10.
my_tail11([_|TL],TL).
my_msort12(A,B):-msort(A,B).
my_set13(A):-list_to_set(A,A).
my_even14(A):-0 is A mod 2.
my_pred15(A,B):-succ(B,A),A > 0.
my_max_list16(A,B):-max_list(A,B).
my_element17(A,B):-member(B,A).
my_min_list18(A,B):-min_list(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_sumlist4,[list(int),int]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_last6,[list(T),T]).
prim(my_len7,[list(_),int]).
prim(my_toupper8,[char,char]).
prim(my_succ9,[int,int]).
prim(my_double10,[int,int]).
prim(my_tail11,[list(T),list(T)]).
prim(my_msort12,[list(int),list(int)]).
prim(my_set13,[list(_)]).
prim(my_even14,[int]).
prim(my_pred15,[int,int]).
prim(my_max_list16,[list(int),int]).
prim(my_element17,[list(T),T]).
prim(my_min_list18,[list(int),int]).
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
p([x,'M','O','M','S',l,'K',q],[m,o,m,s,k]).
p([g,'L','A','P','J',y],[l,a,p,j]).
p([e,'S',k,g,a],[s]).
p([b,f,'Y',q],[y]).
p([r,'V',o,r,'U','K',t,f],[v,u,k]).
q([y,y,l,'T',q,'T','D'],[t,t,d,'X']).
q(['J',h,'M','Z','W','Q'],[m,j,'W',w,z,q]).
q(['X',t,q,l,o],[x,'N']).
q(['O',o,'F',t,r],[f,o,'F']).
q([c,e,v,v,'P','E',r,'U',z],[e,p,w,u]).
