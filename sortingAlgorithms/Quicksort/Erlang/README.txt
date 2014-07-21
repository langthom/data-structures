--- Using this code ---

To use this code, follow this instructions:

$> erlc main.erl quickSort.erl
$> erl

In the erlang shell then do:

1> l(main).
{module,main}

2> List = [1,2,999,85,0,45,-5,732,8,0,15].
[1,2,999,85,0,45,-5,732,8,0,15]

3> main:main(List).
Sorted list: -5 0 1 2 8 15 45 85 732 999 
ok


End the erlang shell via:

4> q().
ok


