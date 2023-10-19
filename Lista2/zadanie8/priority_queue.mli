type 'a priority_queue

val pq_create : 'a priority_queue;;

val pq_add_elem : 'a -> 'a priority_queue -> 'a priority_queue

val pq_min : 'a priority_queue -> 'a option

val pq_rm_elem : 'a priority_queue -> 'a priority_queue

val pq_concat : 'a priority_queue -> 'a priority_queue -> 'a priority_queue