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

my_flatten4(A,B):-flatten(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_pred6(A,B):-succ(B,A),A > 0.
my_min_list7(A,B):-min_list(A,B).
my_msort8(A,B):-msort(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_toupper10(A,B):-upcase_atom(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_odd13(A):-1 is A mod 2.
my_max_list14(A,B):-max_list(A,B).
my_len15(A,B):-length(A,B).
my_element16(A,B):-member(B,A).
my_last17(A,B):-last(A,B).
my_reverse18(A,B):-reverse(A,B).
my_set19(A):-list_to_set(A,A).
my_even20(A):-0 is A mod 2.
my_list_to_set21(A,B):-list_to_set(A,B).
my_tail22([_|TL],TL).
my_head23([H|_],H).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_lowercase5,[char]).
prim(my_pred6,[int,int]).
prim(my_min_list7,[list(int),int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_double9,[int,int]).
prim(my_toupper10,[char,char]).
prim(my_sumlist11,[list(int),int]).
prim(my_succ12,[int,int]).
prim(my_odd13,[int]).
prim(my_max_list14,[list(int),int]).
prim(my_len15,[list(_),int]).
prim(my_element16,[list(T),T]).
prim(my_last17,[list(T),T]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_set19,[list(_)]).
prim(my_even20,[int]).
prim(my_list_to_set21,[list(T),list(T)]).
prim(my_tail22,[list(T),list(T)]).
prim(my_head23,[list(T),T]).
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
p([q,p,'B','T'],[b,t]).
p(['S',h,q,'X',z,'G',d,z,a],[s,x,g]).
p([a,b,l,l,v],[]).
p(['A','J',n,h,j],[a,j]).
p(['E','C',b,w,g,o,z,'E'],[e,c,e]).
q([z,n,z,'N','S','A',h,s,v],[n,s,a,h]).
q(['O','G','S','E','J','J'],[e,j,s,g,j,m,o]).
q([c,'K','L',y,'D',f,h,h],['A',l,d,k]).
q([c,'W',g,p,'C',w,'A','D'],[a,'B',d,w,c]).
q(['Y','X',g,'Z','T',s],[t,x,z,l,y]).
