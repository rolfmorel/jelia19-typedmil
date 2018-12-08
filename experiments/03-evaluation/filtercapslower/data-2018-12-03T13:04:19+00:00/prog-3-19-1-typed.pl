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

my_set4(A):-list_to_set(A,A).
my_pred5(A,B):-succ(B,A),A > 0.
my_list_to_set6(A,B):-list_to_set(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_flatten8(A,B):-flatten(A,B).
my_len9(A,B):-length(A,B).
my_odd10(A):-1 is A mod 2.
my_sumlist11(A,B):-sumlist(A,B).
my_min_list12(A,B):-min_list(A,B).
my_tail13([_|TL],TL).
my_lowercase14(A):-downcase_atom(A,A).
my_head15([H|_],H).
my_reverse16(A,B):-reverse(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
my_even18(A):-0 is A mod 2.
my_last19(A,B):-last(A,B).
my_msort20(A,B):-msort(A,B).
my_max_list21(A,B):-max_list(A,B).
my_succ22(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_set4,[list(_)]).
prim(my_pred5,[int,int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_toupper7,[char,char]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_len9,[list(_),int]).
prim(my_odd10,[int]).
prim(my_sumlist11,[list(int),int]).
prim(my_min_list12,[list(int),int]).
prim(my_tail13,[list(T),list(T)]).
prim(my_lowercase14,[char]).
prim(my_head15,[list(T),T]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_double17,[int,int]).
prim(my_even18,[int]).
prim(my_last19,[list(T),T]).
prim(my_msort20,[list(int),list(int)]).
prim(my_max_list21,[list(int),int]).
prim(my_succ22,[int,int]).
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
p([i,m,p,h,'U'],[u]).
p(['O',c,'S',t,'S','D','Y','Y'],[o,s,s,d,y,y]).
p([e,'U',f,x,'N','Y',o,'U'],[u,n,y,u]).
p(['C','D','R','E','H'],[c,d,r,e,h]).
p([c,f,c,'T'],[t]).
q([p,l,'R',t],['H',r]).
q([k,'B',t,i,'Q'],[b,q,m]).
q(['S','N','T',t,'T','V','X'],[v,t,'Y',s,t,x,n]).
q([u,'U',l,'T'],[u,t,t]).
q([d,'V','A','R',r,'R','C','D','N'],[r,d,c,v,n,a,m,r]).
