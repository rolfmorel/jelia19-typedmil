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
my_odd5(A):-1 is A mod 2.
my_max_list6(A,B):-max_list(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_set8(A):-list_to_set(A,A).
my_tail9([_|TL],TL).
my_flatten10(A,B):-flatten(A,B).
my_reverse11(A,B):-reverse(A,B).
my_element12(A,B):-member(B,A).
my_min_list13(A,B):-min_list(A,B).
my_msort14(A,B):-msort(A,B).
my_even15(A):-0 is A mod 2.
my_head16([H|_],H).
my_lowercase17(A):-downcase_atom(A,A).
my_len18(A,B):-length(A,B).
my_toupper19(A,B):-upcase_atom(A,B).
my_list_to_set20(A,B):-list_to_set(A,B).
my_last21(A,B):-last(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_sumlist4,[list(int),int]).
prim(my_odd5,[int]).
prim(my_max_list6,[list(int),int]).
prim(my_succ7,[int,int]).
prim(my_set8,[list(_)]).
prim(my_tail9,[list(T),list(T)]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_element12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_msort14,[list(int),list(int)]).
prim(my_even15,[int]).
prim(my_head16,[list(T),T]).
prim(my_lowercase17,[char]).
prim(my_len18,[list(_),int]).
prim(my_toupper19,[char,char]).
prim(my_list_to_set20,[list(T),list(T)]).
prim(my_last21,[list(T),T]).
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
p(['G',b,'S',a,i,'V'],[g,s,v]).
p([d,'A',z,'U',n,'C',k],[a,u,c]).
p(['Z','C',w,'N'],[z,c,n]).
p(['Z','B','D','U',t],[z,b,d,u]).
p([a,k,'D',i,'D','Q','P',b,'B'],[d,d,q,p,b]).
q(['V','Y','L','M',z,'U',v,q,r],[m,v,l,'N',u,y]).
q(['B',m,l,c,'G',a,t,i],[b,g,'B']).
q([j,w,q,k,i,'N','O'],[n,o,z]).
q([a,e,'F',q,x],['E',f]).
q(['X',f,g,'O','L','G',w],[o,g,l,x,'I']).
